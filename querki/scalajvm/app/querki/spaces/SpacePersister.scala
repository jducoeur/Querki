package querki.spaces

import akka.actor._

import anorm.{Success=>AnormSuccess,_}
import anorm.SqlParser._

import play.api.db._

import org.querki.requester._

import models.{OID, UnknownOID}
import models.{Collection, Property, PType, PTypeBuilder, SimplePTypeBuilder, Kind, Thing, ThingState, Wikitext}
import models.Kind._
import models.MIMEType.MIMEType
import models.Thing.PropMap

import querki.cluster.OIDAllocator._
import querki.db._
import ShardKind._
import querki.ecology._
import querki.evolutions.Evolutions
import querki.globals._
import querki.identity.{User}
import querki.time._
import querki.time.TimeAnorm._
import querki.types.ModelTypeDefiner
import querki.values.{ElemValue, QLContext, QValue, SpaceState}
import querki.util.SqlHelpers.{oid => oidParser, _}

import PersistMessages._

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
private [spaces] class SpacePersister(val id:OID, implicit val ecology:Ecology) extends Actor with EcologyMember 
  with Requester with SpaceLoader with ModelTypeDefiner 
{
  
  lazy val Core = interface[querki.core.Core]
  lazy val Evolutions = interface[querki.evolutions.Evolutions]
  lazy val QuerkiCluster = interface[querki.cluster.QuerkiCluster]
  lazy val SpacePersistence = interface[querki.spaces.SpacePersistence]
  lazy val SystemInterface = interface[querki.system.System]
  lazy val Types = interface[querki.types.Types]
  lazy val UserAccess = interface[querki.identity.UserAccess]
  
  // For the moment, new-style object creation is only turned on if it says so in config. This is
  // so that we can conduct lower-risk experiments in production before turning it on.
  lazy val newObjCreate = Config.getBoolean("querki.cluster.newObjCreate", false)

  // The OID of the Space, based on the sid
  def oid = Space.oid _
  // The name of the Space's History Table
  def historyTable(id:OID) = "h" + sid(id)
  
  def SpaceSQL(query:String):SqlQuery = SpacePersistence.SpaceSQL(id, query)
  
  // Necessary in order to serialize attachments properly below:
  implicit object byteArrayToStatement extends ToStatement[Array[Byte]] {
    def set(s: java.sql.PreparedStatement, i: Int, array: Array[Byte]): Unit = {
      s.setBlob(i, new javax.sql.rowset.serial.SerialBlob(array))
    }
  }
  
  case class SpaceInfoInternal(name:String, owner:OID, version:Int)
  
  private val spaceInfoParser =
    str("name") ~ oidParser("owner") ~ int("version") map
      { case name ~ owner ~ version => SpaceInfoInternal(name, owner, version) }
  
  /**
   * This is a var instead of a lazy val, because the name can change at runtime.
   * 
   * TODO: bundle this into the overall state parameter, as described in _currentState.
   * Is there any info here that isn't part of the SpaceState?
   */
  var _currentSpaceInfo:Option[SpaceInfoInternal] = None
  /**
   * Fetch the high-level information about this Space. Note that this will throw
   * an exception if for some reason we can't load the record. Re-run this if you
   * have reason to believe the Spaces record has been changed. 
   */
  def fetchSpaceInfo() = {
    _currentSpaceInfo = Some(QDB(System) { implicit conn =>
      SQL("""
          select * from Spaces where id = {id}
          """)
        .on("id" -> id.raw)
        .as(spaceInfoParser.single)
    })
  }
  def spaceInfo:SpaceInfoInternal = {
    if (_currentSpaceInfo.isEmpty) fetchSpaceInfo()
    _currentSpaceInfo.get
  }
  def name = spaceInfo.name
  def owner = spaceInfo.owner
  def version = spaceInfo.version
  
  /**
   * The literal contents of a row in a Space's table.
   */
  private case class RawSpaceRow(kind:Int, propStr:String, modified:DateTime, id:OID, model:OID)
  private val rawSpaceParser =
    int("kind") ~ str("props") ~ dateTime("modified") ~ oidParser("id") ~ oidParser("model") map
      to(RawSpaceRow)

  def receive = {
    
    /***************************/
    
    case Evolve => {
      // NOTE: this can take a long time! This is the point where we evolve the Space to the
      // current version:
      Evolutions.checkEvolution(id, version)
      
      sender ! Evolved
    }
    
    case GetOwner => {
      sender ! SpaceOwner(owner)
    }
    
    case Clear => {
      _currentSpaceInfo = None
    }
    
    case Load(apps) => {
	    // TODO: we need to walk up the tree and load any ancestor Apps before we prep this Space
	    QDB(ShardKind.User) { implicit conn =>
	      // Check whether the table exists
	      // Spaces created since the introduction of Akka Persistence won't have tables here, so we
	      // need to now check explicitly.
	      val spacesFound = SpaceSQL("""
	          SELECT COUNT(*) as count
	          FROM information_schema.tables
	          WHERE table_name = '{tname}'
	          """)
	        .on("dbname" -> ShardKind.dbName(ShardKind.User))
	        .as(long("count").single)
	      if (spacesFound > 0) {
  	      // The list of all of the Things in this Space.
  	      // Note that this is pretty inefficient in terms of memory -- we're going to be copying
  	      // the rows several times. This is considered acceptable only because this code is
  	      // deprecated, and will be entirely replaced by Akka Persistence soon.
  	      val raws = SpaceSQL("""
  	          select * from {tname} where deleted = FALSE
  	          """)
  	        .as(rawSpaceParser.*)
  	      if (raws.isEmpty) {
  	        // This was created with *zero* rows, not even the Space itself, so it is new-style:
  	        sender ! NoOldSpace
  	      } else {
  	        // Old-style Space:
    	      // Split the stream, dividing it by Kind:
    	      val rawsByKind = raws.groupBy(_.kind)
    	      
    	      val loader = new ThingStreamLoader {
    		      // Now load each kind. We do this in order, although in practice it shouldn't
    		      // matter too much so long as Space comes last:
    		      def getThingList[T <: Thing](kind:Int)(state:SpaceState)(builder:(OID, OID, PropMap, DateTime) => T):List[T] = {
    		        rawsByKind.get(kind).getOrElse(List.empty).map({ raw =>
    		          // This is a critical catch, where we log load-time errors. But we don't want to
    		          // raise them to the user, so objects that fail to load are (for the moment) quietly
    		          // suppressed.
    		          // TBD: we should get more refined about these errors, and expose them a bit
    		          // more -- as it is, errors can propagate widely, so objects just vanish. 
    		          // But they should generally be considered internal errors.
    		          try {
    		            Some(
    		              builder(
    		                raw.id, 
    		                raw.model, 
    		                SpacePersistence.deserializeProps(raw.propStr, state),
    		                raw.modified))
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
    	      }
    
    	      val state = doLoad(loader, apps)
    	      sender ! Loaded(state)
  	      }
	      } else {
	        sender ! NoOldSpace
	      }
	    }
    }
    
    /***************************/
    
    case Delete(thingId) => {
      QDB(ShardKind.User) { implicit conn =>
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
    
    case Change(state, thingId, modelId, modTime, props, spaceChangeOpt) => {
      QDB(ShardKind.User) { implicit conn =>
        SpaceSQL("""
          UPDATE {tname}
          SET model = {modelId}, modified = {modified}, props = {props}
          WHERE id = {thingId}
          """
          ).on("thingId" -> thingId.raw,
               "modelId" -> modelId.raw,
               "modified" -> modTime,
               "props" -> SpacePersistence.serializeProps(props, state)).executeUpdate()
      }
      
      spaceChangeOpt map { spaceChange =>
        // In principle, this ought to be in the same transaction, but we
        // don't currently have a mechanism for cross-DB transactions:
        QDB(System) {implicit conn =>
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
               
      sender ! Changed(thingId, modTime)
    }
    
    /***************************/
    
    /**
     * This used to be part of Change. It has been broken out, and is now the only non-vestigial part
     * of this Actor.
     * 
     * For the moment, this is purely fire-and-forget, and doesn't respond. Possibly it should do so.
     */
    case SpaceChange(newName, newDisplay) => {
        QDB(System) {implicit conn =>
          SQL("""
            UPDATE Spaces
            SET name = {newName}, display = {displayName}
            WHERE id = {thingId}
            """
          ).on("newName" -> newName, 
               "thingId" -> id.raw,
               "displayName" -> newDisplay).executeUpdate()          
        }
    }
    
    /***************************/
    
    case Create(state:SpaceState, modelId:OID, kind:Kind, props:PropMap, modTime:DateTime) => {
      allocThingId() map { thingId =>
        QDB(ShardKind.User) { implicit conn =>
          // TODO: add a history record
          SpacePersistence.createThingInSql(thingId, id, modelId, kind, props, modTime, state)
        }          
        sender ! Changed(thingId, modTime)
      }
    }
  }
  
  // TEMP: while we're transitioning to the new Akka Persistence world, we have both OID-creation
  // approaches in the code. Once we're confident that it's all working right, remove the
  // OID.next version, and maybe just move the QuerkiCluster call back inline again:
  def allocThingId():RequestM[OID] = {
    if (newObjCreate)
      QuerkiCluster.oidAllocator.request(NextOID).map { case NewOID(thingId) => thingId }
    else
      RequestM.successful(OID.next(ShardKind.User))
  }
}

object SpacePersister {
}