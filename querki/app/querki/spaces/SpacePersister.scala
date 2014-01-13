package querki.spaces

import akka.actor._

import anorm.{Success=>AnormSuccess,_}

import play.api.db._
import play.api.Play.current

import models.{OID, UnknownOID}
import models.{Attachment, Collection, Property, PType, PTypeBuilder, SimplePTypeBuilder, Kind, Thing, ThingState, Wikitext}
import models.Kind._
import models.MIMEType.MIMEType
import models.Thing._
import models.system.{SystemType}

import querki.ecology._
import querki.time._
import querki.time.TimeAnorm._

import querki.db.ShardKind
import ShardKind._
import querki.evolutions.Evolutions
import querki.identity.User
import querki.values.{ElemValue, QLContext, QValue, SpaceState}
import querki.util._
import querki.util.SqlHelpers._

import PersistMessages._
import messages.AttachmentContents

/**
 * This actor manages the actual persisting of a Space to and from the database. This code
 * was originally contained in the Space itself, but has been pulled out into its own Actor
 * so as to reduce blocking in the main Space Actor, and to abstract things away for testing.
 * 
 * *All* Space-specific code that talks to the database should go through here! That way,
 * we can unit-test Space itself without any DB dependencies, and we have a nice bottleneck
 * to replace if and when we change the persistence model.
 * 
 * Note the implication: this sucker does a *lot* of blocking, and asks to here are likely to
 * be bottlenecks. So use ask with caution, preferably using the Requester pattern. There is
 * one SpacePersister per Space, so they don't interfere with each other.
 * 
 * The Persister does not maintain its own State. Instead, it works hand-in-glove with the
 * Space itself to manage that.
 * 
 * Write requests are, by and large, best-effort.
 * 
 * TODO: The medium-term plan is that, if this throws any exceptions, it should result in
 * killing the Space. This should be mediated through the SpaceManager, probably, or there
 * might be a mid-level Actor between the SpaceManager and Space/SpacePersister, which
 * manages the whole hive of Actors for this Space. (That might be the better architecture,
 * now that I think of it.)
 */
private [spaces] class SpacePersister(val id:OID, implicit val ecology:Ecology) extends Actor with EcologyMember {
  
  lazy val SystemInterface = interface[querki.system.System]
  lazy val Core = interface[querki.core.Core]
  lazy val SpacePersistence = interface[querki.spaces.SpacePersistence]
  lazy val Evolutions = interface[querki.evolutions.Evolutions]

  // The OID of the Space, based on the sid
  def oid = Space.oid _
  // The name of the Space's History Table
  def historyTable(id:OID) = "h" + sid(id)
  
  def SpaceSQL(query:String):SqlQuery = SpacePersistence.SpaceSQL(id, query)
  def AttachSQL(query:String):SqlQuery = SpacePersistence.AttachSQL(id, query)
  
  // TODO: this sort of state just plain doesn't belong here...
  
  /**
   * This is a var instead of a lazy val, because the name can change at runtime.
   * 
   * TODO: bundle this into the overall state parameter, as described in _currentState.
   * Is there any info here that isn't part of the SpaceState?
   */
  var _currentSpaceInfo:Option[SqlRow] = None
  /**
   * Fetch the high-level information about this Space. Note that this will throw
   * an exception if for some reason we can't load the record. Re-run this if you
   * have reason to believe the Spaces record has been changed. 
   */
  def fetchSpaceInfo() = {
    _currentSpaceInfo = Some(DB.withTransaction(dbName(System)) { implicit conn =>
      SQL("""
          select * from Spaces where id = {id}
          """).on("id" -> id.raw).apply().headOption.get
    })
  }
  def spaceInfo:SqlRow = {
    if (_currentSpaceInfo.isEmpty) fetchSpaceInfo()
    _currentSpaceInfo.get
  }
  def name = spaceInfo.get[String]("name").get
  def owner = OID(spaceInfo.get[Long]("owner").get)
  def version = spaceInfo.get[Int]("version").get
  
  def receive = {
    
    /***************************/
    
    case Load => {
	    // NOTE: this can take a long time! This is the point where we evolve the Space to the
	    // current version:
	    Evolutions.checkEvolution(id, version)
	    
	    // TODO: we need to walk up the tree and load any ancestor Apps before we prep this Space
	    DB.withTransaction(dbName(ShardKind.User)) { implicit conn =>
	      // The stream of all of the Things in this Space:
	      val stateStream = SpaceSQL("""
	          select * from {tname} where deleted = FALSE
	          """)()
	      // Split the stream, dividing it by Kind:
	      val streamsByKind = stateStream.groupBy(_.get[Int]("kind").get)
	      
	      // Start off using the App to boot this Space. Then we add each aspect as we read it in.
	      // This works decently for now, but will fall afoul when we try to have local meta-Properties;
	      // those will wind up with pointer errors.
	      // TODO: Do the Property load in multiple phases, so we can have local meta-properties.
	      // TODO: this should use the App, not SystemSpace:
	      var curState:SpaceState = SystemInterface.State
	      
	      // Now load each kind. We do this in order, although in practice it shouldn't
	      // matter too much so long as Space comes last:
	      def getThingStream[T <: Thing](kind:Int)(builder:(OID, OID, PropMap, DateTime) => T):Stream[T] = {
	        streamsByKind.get(kind).getOrElse(Stream.Empty).map({ row =>
	          // This is a critical catch, where we log load-time errors. But we don't want to
	          // raise them to the user, so objects that fail to load are (for the moment) quietly
	          // suppressed.
	          // TBD: we should get more refined about these errors, and expose them a bit
	          // more -- as it is, errors can propagate widely, so objects just vanish. 
	          // But they should generally be considered internal errors.
	          try {
	            val propMap = SpacePersistence.deserializeProps(row.get[String]("props").get, curState)
	            val modTime = row.get[DateTime]("modified").get
	            Some(
	              builder(
	                OID(row.get[Long]("id").get), 
	                OID(row.get[Long]("model").get), 
	                propMap,
	                modTime))
	          } catch {
	            case error:Exception => {
	              // TODO: this should go to a more serious error log, that we pay attention to. It
	              // indicates an internal DB inconsistency that we should have ways to clean up.
	              QLog.error("Error while trying to load ThingStream " + id, error)
	              None
	            }            
	          }
	        }).flatten
	      }
	      
	      def getThings[T <: Thing](kind:Int)(builder:(OID, OID, PropMap, DateTime) => T):Map[OID, T] = {
	        val tStream = getThingStream(kind)(builder)
	        (Map.empty[OID, T] /: tStream) { (m, t) =>
	          try {
	            m + (t.id -> t)
	          } catch {
	            case error:Exception => {
	              QLog.error("Error while trying to assemble ThingStream " + id, error)
	              m
	            }
	          }
	        }
	      }
	      
	      val spaceStream = getThingStream(Kind.Space) { (thingId, modelId, propMap, modTime) =>
	        new SpaceState(
	             thingId,
	             modelId,
	             () => propMap,
	             owner,
	             name,
	             modTime,
	             Some(SystemInterface.State),
	             // TODO: dynamic PTypes
	             Map.empty[OID, PType[_]],
	             Map.empty[OID, Property[_,_]],
	             Map.empty[OID, ThingState],
	             // TODO (probably rather later): dynamic Collections
	             Map.empty[OID, Collection],
	             None,
	             ecology
	            )
	      }
	      
	      curState =
	        if (spaceStream.isEmpty) {
	          // This wants to be a Big Nasty Error!
	          QLog.error("Was unable to find/load Space " + id + "/" + name + ". INVESTIGATE THIS!")
	          
	          // In the meantime, we fall back on a plain Space Thing:
	          new SpaceState(
	            id,
	            SystemInterface.State.id,
	            toProps(
	              Core.setName(name),
	              interface[querki.basic.Basic].DisplayTextProp("We were unable to load " + name + " properly. An error has been logged; our apologies.")
	              ),
	            owner,
	            name,
	            epoch,
	            Some(SystemInterface.State),
	            Map.empty[OID, PType[_]],
	            Map.empty[OID, Property[_,_]],
	            Map.empty[OID, ThingState],
	            // TODO (probably rather later): dynamic Collections
	            Map.empty[OID, Collection],
	            None,
	            ecology
	            )
	        } else
	          spaceStream.head
	      
	      val loadedProps = getThings(Kind.Property) { (thingId, modelId, propMap, modTime) =>
	        val typ = SystemInterface.State.typ(Core.TypeProp.first(propMap))
	        // This cast is slightly weird, but safe and should be necessary. But I'm not sure
	        // that the PTypeBuilder part is correct -- we may need to get the RT correct.
	//        val boundTyp = typ.asInstanceOf[PType[typ.valType] with PTypeBuilder[typ.valType, Any]]
	        val boundTyp = typ.asInstanceOf[PType[Any] with PTypeBuilder[Any, Any]]
	        val coll = SystemInterface.State.coll(Core.CollectionProp.first(propMap))
	        // TODO: this feels wrong. coll.implType should be good enough, since it is viewable
	        // as Iterable[ElemValue] by definition, but I can't figure out how to make that work.
	        val boundColl = coll.asInstanceOf[Collection]
	        new Property(thingId, id, modelId, boundTyp, boundColl, () => propMap, modTime)
	      }
	      curState = curState.copy(spaceProps = loadedProps)
	      
	      val things = getThings(Kind.Thing) { (thingId, modelId, propMap, modTime) =>
	        new ThingState(thingId, id, modelId, () => propMap, modTime)        
	      }
	      
	      val attachments = getThings(Kind.Attachment) { (thingId, modelId, propMap, modTime) =>
	        new Attachment(thingId, id, modelId, () => propMap, modTime)        
	      }
	      
	      val allThings = things ++ attachments
	      curState = curState.copy(things = allThings)
	      
	      // Now we do a second pass, to resolve anything left unresolved:
	      def secondPassProps[T <: Thing](thing:T)(copier:(T, PropMap) => T):T = {
	        val fixedProps = thing.props.map { propPair =>
	          val (id, value) = propPair
	          value match {
	            case unres:UnresolvedPropValue => {
	              val propOpt = curState.prop(id)
	              val v = propOpt match {
	                case Some(prop) => prop.deserialize(value.firstTyped(SpacePersistence.UnresolvedPropType).get)
	                case None => value
	              }
	              (id, v)
	            }
	            case _ => propPair
	          }
	        }
	        copier(thing, fixedProps)
	      }
	
	      curState = secondPassProps(curState)((state, props) => state.copy(pf = () => props))
	      
	      val fixedAllProps = curState.spaceProps.map{ propPair =>
	        val (id, prop) = propPair
	        (id, secondPassProps(prop)((p, metaProps) => p.copy(pf = () => metaProps)))
	      }.toSeq
	      curState = curState.copy(spaceProps = Map(fixedAllProps:_*))
	      
	      // BLOCKING, but useful: make the owner visible, so that we can, eg, write URLs
	      curState = curState.copy(ownerIdentity = User.getIdentity(owner))
	      
	      sender ! Loaded(curState)
	    }
    }
    
    /***************************/
    
    case Delete(thingId) => {
      DB.withTransaction(dbName(ShardKind.User)) { implicit conn =>
	    // TODO: add a history record
	    SpaceSQL("""
	      UPDATE {tname}
	      SET deleted = TRUE
	      WHERE id = {thingId}
	      """
	    ).on("thingId" -> thingId.raw).executeUpdate()
      }      
    }
    
    /***************************/
    
    case Change(state, thingId, modelId, props, spaceChangeOpt) => {
      DB.withTransaction(dbName(ShardKind.User)) { implicit conn =>
        SpaceSQL("""
          UPDATE {tname}
          SET model = {modelId}, props = {props}
          WHERE id = {thingId}
          """
          ).on("thingId" -> thingId.raw,
               "modelId" -> modelId.raw,
               "props" -> SpacePersistence.serializeProps(props, state)).executeUpdate()
               
        // TODO: in principle, this should come from the DB's timestamp:
        val modTime = DateTime.now
        sender ! Changed(thingId, modTime)
      }
      
      spaceChangeOpt map { spaceChange =>
        // In principle, this ought to be in the same transaction, but we
        // don't currently have a mechanism for cross-DB transactions:
        DB.withTransaction(dbName(ShardKind.System)) {implicit conn =>
          SQL("""
            UPDATE Spaces
            SET name = {newName}, display = {displayName}
            WHERE id = {thingId}
            """
          ).on("newName" -> spaceChange.newName, 
               "thingId" -> thingId.raw,
               "displayName" -> spaceChange.newDisplay).executeUpdate()          
        }
      }
    }
    
    /***************************/
    
    case Create(state:SpaceState, modelId:OID, kind:Kind, props:PropMap, attachmentInfo:Option[AttachmentInfo]) => {
      DB.withTransaction(dbName(ShardKind.User)) { implicit conn =>
        val thingId = OID.next(ShardKind.User)
        // TODO: add a history record
        SpacePersistence.createThingInSql(thingId, id, modelId, kind, props, state)
        // TBD: this isn't quite right -- we really should be taking the DB's definition of the timestamp
        // instead:
        val modTime = DateTime.now
      
        attachmentInfo.map { info =>
          val AttachmentInfo(content, mime, size) = info
          AttachSQL("""
            INSERT INTO {tname}
            (id, mime, size, content) VALUES
            ({thingId}, {mime}, {size}, {content})
          """
          ).on("thingId" -> thingId.raw,
               "mime" -> mime,
               "size" -> size,
               "content" -> content).executeUpdate()       
        }
        
        sender ! Changed(thingId, modTime)
      }
    }
    
    /***************************/
    
    case LoadAttachment(attachOid:OID) => {
      DB.withTransaction(dbName(ShardKind.User)) { implicit conn =>
        // TODO: this will throw an error if the specified attachment doesn't exist
        // Guard against that.
        val results = AttachSQL("""
            SELECT mime, size, content FROM {tname} where id = {id}
            """).on("id" -> attachOid.raw)().map {
          // Note: this weird duplication is a historical artifact, due to the fact
          // that some of the attachment tables have "content" as a nullable column,
          // and some don't. We may eventually want to evolve everything into
          // consistency...
          case Row(Some(mime:MIMEType), Some(size:Int), Some(content:Array[Byte])) => {
            AttachmentContents(attachOid, size, mime, content)
          }
          case Row(mime:MIMEType, size:Int, Some(content:Array[Byte])) => {
            AttachmentContents(attachOid, size, mime, content)
          }
          case Row(mime:MIMEType, size:Int, content:Array[Byte]) => {
            AttachmentContents(attachOid, size, mime, content)
          }
        }.head
        sender ! results
      }      
    }
  }
}

object SpacePersister {
}