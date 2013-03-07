package models

import scala.concurrent.duration._
import scala.concurrent.Future

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import play.api.libs.concurrent.Execution.Implicits._

import play.api._
import play.api.Configuration
import play.api.Play
import play.api.Play.current
import play.api.libs.concurrent._
import play.Configuration

// Database imports:
import anorm._
import play.api.db._
import play.api.Play.current

import Kind._
import Thing._

import system._
import system.OIDs._
import system.SystemSpace._

import MIMEType.MIMEType

/**
 * The Actor that encapsulates a Space.
 * 
 * As a hard and fast rule, application code in Querki never alters a Space directly.
 * Instead, all changes are described as messages to the Space's Actor; that is
 * responsible for making the actual changes. This way, the database layer is firmly
 * encapsulated, and race conditions are prevented.
 * 
 * Similarly, you fetch the Space's current State from the Space Actor. The State is
 * an immutable object completely describing the Space at a single moment; you are
 * allowed to process that in any way, just not change it.
 * 
 * An Actor's name is based on its OID, as is its Thing Table in the DB. Use Space.sid()
 * to get the name. Note that this has *nothing* to do with the Space's Display Name, which
 * is user-defined. (And unique only to that user.)
 */
class Space extends Actor {
  
  import context._
  import models.system.SystemSpace.{State => systemState, _} 
  
  def id = OID(self.path.name)
  var _currentState:Option[SpaceState] = None
  // This should return okay any time after preStart:
  def state = {
    _currentState match {
      case Some(s) => s
      case None => throw new Exception("State not ready in Actor " + id)
    }
  }
  // TODO: keep the previous states here in the Space, so we can undo/history
  def updateState(newState:SpaceState) = {
    _currentState = Some(newState)
  }
  
  def SpaceSQL(query:String) = Space.SpaceSQL(id, query)
  def AttachSQL(query:String) = Space.AttachSQL(id, query)

  /**
   * This is a var instead of a lazy val, because the name can change at runtime.
   */
  var _currentSpaceInfo:Option[SqlRow] = None
  /**
   * Fetch the high-level information about this Space. Note that this will throw
   * an exception if for some reason we can't load the record. Re-run this if you
   * have reason to believe the Spaces record has been changed. 
   */
  def fetchSpaceInfo() = {
    _currentSpaceInfo = Some(DB.withTransaction { implicit conn =>
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
  
  // TODO: this needs to become real. For now, everything is world-readable.
  def canRead(who:OID):Boolean = {
    true
  }
  
  // TODO: this needs to become much more sophisticated. But for now, it's good enough
  // to say that only the owner can edit:
  def canCreateThings(who:OID):Boolean = {
    who == owner
  }
  
  // TODO: this needs to become much more sophisticated. But for now, it's good enough
  // to say that only the owner can edit:
  def canEdit(who:OID, thingId:OID):Boolean = {
    who == owner
  }
  
  def loadSpace() = {
    // TODO: we need to walk up the tree and load any ancestor Apps before we prep this Space
    DB.withTransaction { implicit conn =>
      // The stream of all of the Things in this Space:
      val stateStream = SpaceSQL("""
          select * from {tname}
          """)()
      // Split the stream, dividing it by Kind:
      val streamsByKind = stateStream.groupBy(_.get[Int]("kind").get)
      
      // Start off using the App to boot this Space. Then we add each aspect as we read it in.
      // This works decently for now, but will fall afoul when we try to have local meta-Properties;
      // those will wind up with pointer errors.
      // TODO: Do the Property load in multiple phases, so we can have local meta-properties.
      // TODO: this should use the App, not SystemSpace:
      var curState:SpaceState = systemState
      
      // Now load each kind. We do this in order, although in practice it shouldn't
      // matter too much so long as Space comes last:
      def getThingStream[T <: Thing](kind:Int)(builder:(OID, OID, PropMap) => T):Stream[T] = {
        streamsByKind.get(kind).getOrElse(Stream.Empty).map { row =>
          // TODO: the app shouldn't be hardcoded to SystemSpace
          val propMap = Thing.deserializeProps(row.get[String]("props").get, curState)
          builder(OID(row.get[Long]("id").get), OID(row.get[Long]("model").get), propMap)
        }
      }
      def getThings[T <: Thing](kind:Int)(builder:(OID, OID, PropMap) => T):Map[OID, T] = {
        val tStream = getThingStream(kind)(builder)
        (Map.empty[OID, T] /: tStream)((m, t) => m + (t.id -> t))
      }
      
      val spaceStream = getThingStream(Kind.Space) { (thingId, modelId, propMap) =>
        new SpaceState(
             thingId,
             modelId,
             () => propMap,
             owner,
             name,
             Some(systemState),
             // TODO: dynamic PTypes
             Map.empty[OID, PType[_]],
             Map.empty[OID, Property[_,_]],
             Map.empty[OID, ThingState],
             // TODO (probably rather later): dynamic Collections
             Map.empty[OID, Collection]
            )
      }
      // TBD: note that it is a hard error if there aren't any Spaces found. We expect exactly one:
      curState = spaceStream.head
      
      val props = getThings(Kind.Property) { (thingId, modelId, propMap) =>
        val typ = systemState.typ(TypeProp.first(propMap))
        // This cast is slightly weird, but safe and should be necessary. But I'm not sure
        // that the PTypeBuilder part is correct -- we may need to get the RT correct.
//        val boundTyp = typ.asInstanceOf[PType[typ.valType] with PTypeBuilder[typ.valType, Any]]
        val boundTyp = typ.asInstanceOf[PType[Any] with PTypeBuilder[Any, Any]]
        val coll = systemState.coll(CollectionProp.first(propMap))
        // TODO: this feels wrong. coll.implType should be good enough, since it is viewable
        // as Iterable[ElemValue] by definition, but I can't figure out how to make that work.
        val boundColl = coll.asInstanceOf[Collection]
        new Property(thingId, id, modelId, boundTyp, boundColl, () => propMap)
      }
      curState = curState.copy(spaceProps = props)
      
      val things = getThings(Kind.Thing) { (thingId, modelId, propMap) =>
        new ThingState(thingId, id, modelId, () => propMap)        
      }
      
      val attachments = getThings(Kind.Attachment) { (thingId, modelId, propMap) =>
        new ThingState(thingId, id, modelId, () => propMap, Kind.Attachment)        
      }
      
      val allThings = things ++ attachments
      curState = curState.copy(things = allThings)
      
      _currentState = Some(curState)
    }    
  }
  
  override def preStart() = {
    loadSpace()
  } 
  
  def checkSpaceId(thingId:ThingId):OID = {
    thingId match {
      case AsOID(oid) => if (oid == id) oid else throw new Exception("Space " + id + " somehow got message for " + oid)
      case AsName(thingName) => if (NameType.equalNames(thingName, name)) id else throw new Exception("Space " + name + " somehow got message for " + thingName)
    }
  }

  def createSomething(spaceThingId:ThingId, who:OID, modelId:OID, props:PropMap, kind:Kind)(
      otherWork:OID => java.sql.Connection => Unit) = 
  {
    val spaceId = checkSpaceId(spaceThingId)
    val name = NameProp.firstOpt(props)
    if (!canCreateThings(who))
      sender ! ThingFailed("You are not allowed to create anything in this Space")
    else if (name.isDefined && state.anythingByName(name.get).isDefined)
      sender ! ThingFailed("This Space already has a Thing with that name")
    else DB.withTransaction { implicit conn =>
      val thingId = OID.next
      // TODO: add a history record
      Space.createThingInSql(thingId, spaceId, modelId, kind, props, state)
      kind match {
        case Kind.Thing | Kind.Attachment => {
          val thing = ThingState(thingId, spaceId, modelId, () => props, kind)
          updateState(state.copy(things = state.things + (thingId -> thing)))
        }
        case Kind.Property => {
          val typ = state.typ(TypeProp.first(props))
          val coll = state.coll(CollectionProp.first(props))
//          val boundTyp = typ.asInstanceOf[PType[typ.valType] with PTypeBuilder[typ.valType, Any]]
          val boundTyp = typ.asInstanceOf[PType[Any] with PTypeBuilder[Any, Any]]
          val boundColl = coll.asInstanceOf[Collection]
          val thing = Property(thingId, spaceId, modelId, boundTyp, boundColl, () => props)
          updateState(state.copy(spaceProps = state.spaceProps + (thingId -> thing)))          
        }
        case _ => throw new Exception("Got a request to create a thing of kind " + kind + ", but don't know how yet!")
      }
      // This callback intentionally takes place inside the transaction:
      otherWork(thingId)(conn)
        
      sender ! ThingFound(thingId, state)
    }    
  }
  
  def modifyThing(oldThing:ThingState, spaceId:OID, modelId:OID, newProps:PropMap) = {
  }
  
  def receive = {
    case req:CreateSpace => {
      sender ! ThingFound(UnknownOID, state)
    }

    case CreateThing(who, owner, spaceId, kind, modelId, props) => {
      createSomething(spaceId, who, modelId, props, kind) { thingId => implicit conn => Unit }
    }
    
    case CreateAttachment(who, owner, spaceId, content, mime, size, modelId, props) => {
      createSomething(spaceId, who, modelId, props, Kind.Attachment) { thingId => implicit conn =>
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
    }
    
    case GetAttachment(who, owner, space, attachId) => {
      if (!canRead(who))
        sender ! AttachmentFailed
      else DB.withTransaction { implicit conn =>
        val attachOid = attachId match {
          case AsOID(oid) => oid
          // TODO: handle the case where this name is not recognized:
          case AsName(name) => {
            state.anythingByName(name).get.id
          }
        }
        // TODO: this will throw an error if the specified attachment doesn't exist
        // Guard against that.
        val results = AttachSQL("""
            SELECT mime, size, content FROM {tname} where id = {id}
            """).on("id" -> attachOid.raw)().map {
          // TODO: we really, really must not hardcode this type below. This is some sort of
          // serious Anorm driver failure, I think. As it stands, it's going to crash when we
          // move to MySQL:
          case Row(mime:MIMEType, size:Int, content:org.h2.jdbc.JdbcBlob) => {
            AttachmentContents(attachOid, size, mime, content.getBytes(1, content.length().toInt))
          }
        }.head
        sender ! results
      }
    }
    
    case ModifyThing(who, owner, spaceThingId, thingId, modelId, newProps) => {
      Logger.info("In ModifyThing")
      val spaceId = checkSpaceId(spaceThingId)
      val oldThingOpt = state.anything(thingId)
      oldThingOpt map { oldThing =>
        if (!canEdit(who, oldThing.id)) {
          Logger.info(who.toString + " can't edit -- requires " + owner.toString)
          sender ! ThingFailed("You're not allowed to modify that")
        } else {
	      DB.withTransaction { implicit conn =>
	        // TODO: compare properties, build a history record of the changes
	        val thingId = oldThing.id
	        Space.SpaceSQL(spaceId, """
	          UPDATE {tname}
	          SET model = {modelId}, props = {props}
	          WHERE id = {thingId}
	          """
	          ).on("thingId" -> thingId.raw,
	               "modelId" -> modelId.raw,
	               "props" -> Thing.serializeProps(newProps, state)).executeUpdate()    
	        // TODO: this needs a clause for each Kind you can get:
            oldThing match {
              case t:ThingState => {
	            val newThingState = t.copy(m = modelId, pf = () => newProps)
	            updateState(state.copy(things = state.things + (thingId -> newThingState))) 
              }
              case s:SpaceState => {
                // TODO: handle changing the owner or apps of the Space. (Different messages?)
                val rawName = NameProp.first(newProps)
                val newName = NameType.canonicalize(rawName)
                val oldName = NameProp.first(oldThing.props)
                val oldDisplay = DisplayNameProp.firstOpt(oldThing.props) map (_.raw.toString) getOrElse rawName
                val newDisplay = DisplayNameProp.firstOpt(newProps) map (_.raw.toString) getOrElse rawName
                if (!NameType.equalNames(newName, oldName) || !(oldDisplay.contentEquals(newDisplay))) {
                  SQL("""
                    UPDATE Spaces
                    SET name = {newName}, display = {displayName}
                    WHERE id = {thingId}
                    """
                  ).on("newName" -> newName, 
                       "thingId" -> thingId.raw,
                       "displayName" -> newDisplay).executeUpdate()
                }
                updateState(state.copy(m = modelId, pf = () => newProps, name = newName))
              }
	        }
	        sender ! ThingFound(thingId, state)
	      }
          fetchSpaceInfo()
        }
      } getOrElse {
        sender ! ThingFailed("Thing not found")
      }
    }
    
    case GetThing(req, owner, space, thingIdOpt) => {
      if (!canRead(req))
        sender ! ThingFailed("Space not found")
      else if (thingIdOpt.isDefined) {
        val thingOpt = state.anything(thingIdOpt.get)
        if (thingOpt.isDefined) {
          sender ! ThingFound(thingOpt.get.id, state)
        } else {
          // TODO: better error message!
          sender ! ThingFailed("Thing not found")
        }
      } else {
        // TODO: is this the most semantically appropriate response?
        sender ! ThingFound(UnknownOID, state)
      }
    }
  }
}

object Space {
  // The name of the Space Actor
  def sid(id:OID) = id.toString
  // The OID of the Space, based on the sid
  def oid(sid:String) = OID(sid)
  // The name of the Space's Thing Table
  def thingTable(id:OID) = "s" + sid(id)
  // The name of the Space's History Table
  def historyTable(id:OID) = "h" + sid(id)
  // The name of the Space's Attachments Table
  def attachTable(id:OID) = "a" + sid(id)

  /**
   * A Map of Things, suitable for passing into a SpaceState.
   */
  def oidMap[T <: Thing](items:T*):Map[OID,T] = {
    (Map.empty[OID,T] /: items) ((m, i) => m + (i.id -> i))
  }
  
  /**
   * The intent here is to use this with queries that use the thingTable. You can't use
   * on()-style parameters for table names, so we need to work around that.
   * 
   * You can always use this in place of ordinary SQL(); it is simply a no-op for ordinary queries.
   */
  def SpaceSQL(spaceId:OID, query:String):SqlQuery = SQL(query.replace("{tname}", thingTable(spaceId)))
  def createThingInSql(thingId:OID, spaceId:OID, modelId:OID, kind:Int, props:PropMap, serialContext:SpaceState)(implicit conn:java.sql.Connection) = {
    SpaceSQL(spaceId, """
        INSERT INTO {tname}
        (id, model, kind, props) VALUES
        ({thingId}, {modelId}, {kind}, {props})
        """
        ).on("thingId" -> thingId.raw,
             "modelId" -> modelId.raw,
             "kind" -> kind,
             "props" -> Thing.serializeProps(props, serialContext)).executeUpdate()    
  }
  
  def AttachSQL(spaceId:OID, query:String):SqlQuery = SQL(query.replace("{tname}", attachTable(spaceId)))
}
