package querki.notifications

import akka.actor.Props

import anorm.{Success=>AnormSuccess,_}
import play.api.db._
import play.api.Play.current

import querki.db.ShardKind
import ShardKind._
import querki.ecology._
import querki.identity.UserId

object PersistenceMOIDs extends EcotIds(49)

class NotificationPersistenceEcot(e:Ecology) extends QuerkiEcot(e) with NotificationPersistence {
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
  
  def notificationPersisterProps(userId:UserId):Props = NotificationPersister.actorProps(ecology, userId)
}
