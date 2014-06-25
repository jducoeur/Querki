package querki.notifications

import akka.actor.Props

import anorm.{Success=>AnormSuccess,_}
import play.api.db._
import play.api.Play.current

import querki.db.ShardKind
import ShardKind._
import querki.ecology._
import querki.identity.UserId
import querki.time.TimeAnorm._
import querki.util.SqlHelpers._
import querki.values.SpaceState

object PersistenceMOIDs extends EcotIds(49)

class NotificationPersistenceEcot(e:Ecology) extends QuerkiEcot(e) with NotificationPersistence {
  
  lazy val SpacePersistence = interface[querki.spaces.SpacePersistence]
  lazy val SystemInterface = interface[querki.system.System]
  
  lazy val SystemState = SystemInterface.State
  
  // The name of the User's Notification Table
  def noteTable(id:UserId):String = "note" + id.toString
  
  def UserSQL(userId:UserId, query:String, version:Int = 0):SqlQuery = {
    val replQuery = query.
    		replace("{notename}", noteTable(userId))
    SQL(replQuery)
  }
  
  def loadUserInfo(userId:UserId):Option[UserInfo] = {
    DB.withConnection(dbName(System)) { implicit conn =>
      val userStream = SQL("""
          SELECT userVersion from User
           WHERE id = {id} 
      """).on("id" -> userId.raw)()
      userStream.force.map { row =>
        val version = row.get[Int]("userVersion").get
        UserInfo(userId, version)
      }.headOption
    }
  }
  
  def loadCurrent(userId:UserId):CurrentNotifications = {
    DB.withConnection(dbName(User)) { implicit conn =>
      val noteStream = UserSQL(userId, """
          SELECT * from {notename}
           WHERE isDeleted = FALSE
           ORDER BY sentTime DESC
           LIMIT 100
          """)()
      val notes = noteStream.map { row =>
        Notification(
          row.int("id"),
          row.oid("sender"),
          row.optOid("toIdentity"),
          NotifierId(row.short("ecotId"), row.short("noteType")),
          row.dateTime("sentTime"),
          row.optOid("spaceId"),
          row.optOid("thingId"),
          // Do we ever need a specific Space in order to deserialize a Notification? We probably can't,
          // since we are viewing them outside the context of the Space. So just use System.
          SpacePersistence.deserializeProps(row.string("props"), SystemState), 
          row.bool("isRead"), 
          row.bool("isDeleted")
        )
      }
      CurrentNotifications(notes.force)
    }
  }
  
  def notificationPersisterProps(userId:UserId):Props = NotificationPersister.actorProps(ecology, userId)
}
