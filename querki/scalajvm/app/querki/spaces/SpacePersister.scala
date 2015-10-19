package querki.spaces

import akka.actor._

import anorm.{Success=>AnormSuccess,_}

import play.api.db._
import play.api.Play.current

import org.querki.requester._

import models.{OID, UnknownOID}
import models.{Collection, Property, PType, PTypeBuilder, SimplePTypeBuilder, Kind, Thing, ThingState, Wikitext}
import models.Kind._
import models.MIMEType.MIMEType
import models.Thing.PropMap

import querki.cluster.OIDAllocator._
import querki.ecology._
import querki.time._
import querki.time.TimeAnorm._

import querki.db.ShardKind
import ShardKind._
import querki.evolutions.Evolutions
import querki.identity.User
import querki.types.ModelTypeDefiner
import querki.values.{ElemValue, QLContext, QValue, SpaceState}
import querki.util._
import querki.util.SqlHelpers._

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
  
  /**
   * This is a var instead of a lazy val, because the name can change at runtime.
   * 
   * TODO: bundle this into the overall state parameter, as described in _currentState.
   * Is there any info here that isn't part of the SpaceState?
   */
  var _currentSpaceInfo:Option[Row] = None
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
  def spaceInfo:Row = {
    if (_currentSpaceInfo.isEmpty) fetchSpaceInfo()
    _currentSpaceInfo.get
  }
  def name = spaceInfo[String]("name")
  def owner = OID(spaceInfo[Long]("owner"))
  def version = spaceInfo[Int]("version")

  def receive = {
    
    /***************************/
    
    case Evolve => {
      // NOTE: this can take a long time! This is the point where we evolve the Space to the
      // current version:
      Evolutions.checkEvolution(id, version)
      
      sender ! Evolved
    }
    
    case Load => {
	    // TODO: we need to walk up the tree and load any ancestor Apps before we prep this Space
	    DB.withTransaction(dbName(ShardKind.User)) { implicit conn =>
	      // The stream of all of the Things in this Space:
	      val stateStream = SpaceSQL("""
	          select * from {tname} where deleted = FALSE
	          """)()
	      // Split the stream, dividing it by Kind:
	      val streamsByKind = stateStream.groupBy(_[Int]("kind"))
	      
	      val loader = new ThingStreamLoader {
		      // Now load each kind. We do this in order, although in practice it shouldn't
		      // matter too much so long as Space comes last:
		      def getThingStream[T <: Thing](kind:Int)(state:SpaceState)(builder:(OID, OID, PropMap, DateTime) => T):Stream[T] = {
		        streamsByKind.get(kind).getOrElse(Stream.Empty).map({ row =>
		          // This is a critical catch, where we log load-time errors. But we don't want to
		          // raise them to the user, so objects that fail to load are (for the moment) quietly
		          // suppressed.
		          // TBD: we should get more refined about these errors, and expose them a bit
		          // more -- as it is, errors can propagate widely, so objects just vanish. 
		          // But they should generally be considered internal errors.
		          try {
		            val propMap = SpacePersistence.deserializeProps(row[String]("props"), state)
		            val modTime = row[DateTime]("modified")
		            Some(
		              builder(
		                OID(row[Long]("id")), 
		                OID(row[Long]("model")), 
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
	      }

	      val state = doLoad(loader)
	      sender ! Loaded(state)
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
    
    case Change(state, thingId, modelId, modTime, props, spaceChangeOpt) => {
      DB.withTransaction(dbName(ShardKind.User)) { implicit conn =>
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
               
      sender ! Changed(thingId, modTime)
    }
    
    /***************************/
    
    case Create(state:SpaceState, modelId:OID, kind:Kind, props:PropMap, modTime:DateTime) => {
      QuerkiCluster.oidAllocator.request(NextOID) map { case NewOID(thingId) =>
        DB.withTransaction(dbName(ShardKind.User)) { implicit conn =>
          // TODO: add a history record
          SpacePersistence.createThingInSql(thingId, id, modelId, kind, props, modTime, state)
        }          
        sender ! Changed(thingId, modTime)
      }
    }
  }
}

object SpacePersister {
}