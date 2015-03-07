package querki.evolutions

import anorm._
import java.sql.Connection
import play.api.db._
import play.api.Play.current

import models.OID

import querki.db.ShardKind._
import querki.ecology._
import querki.identity.UserId

trait UserStep extends EcologyMember {
  /**
   * Each Step must specify this version stamp, which must be unique!
   */
  def version:Int
  
  implicit def ecology:Ecology
  
  lazy val NotificationPersistence = interface[querki.notifications.NotificationPersistence]
  def UserSQL(userId:OID, query:String, version:Int = 0):SqlQuery = NotificationPersistence.UserSQL(userId, query, version)
  
  def evolveUp(userId:UserId) = {
    // TBD: is there any way to do this all within a single transaction? Since it spans DBs,
    // quite likely not, but as it stands this is a tad riskier than I like:
    val info = DB.withTransaction(dbName(System)) { implicit conn =>
      val row = SQL("""
          select * from User where id = {id}
          """).on("id" -> userId.raw).apply().headOption.get
      UserInfo(row)
    }
    DB.withTransaction(dbName(User)) { implicit spaceConn =>
//      backupTables(info)(spaceConn)
      doEvolve(info)(spaceConn)        
    }
    DB.withTransaction(dbName(System)) { implicit conn =>
      SQL("""
          UPDATE User SET userVersion = {version} WHERE id = {id}
          """).on("version" -> version, "id" -> userId.raw).executeUpdate
    }
  }
  
  /**
   * For the time being at least, we back up the current state of the Space before we
   * evolve it. Eventually, we'll want to get rid of these, but better safe than sorry
   * for now.
   */
  // TBD: should we be using this to back up the Notifications tables?
//  def backupTables(info:SpaceInfo)(implicit conn:Connection):Unit = {
//    // TODO: back up the history and attachments as well?
//    SpacePersistence.SpaceSQL(info.id, """
//        CREATE TABLE {bname} LIKE {tname}
//        """, info.version).executeUpdate
//    SpacePersistence.SpaceSQL(info.id, """
//        INSERT {bname} SELECT * FROM {tname}
//        """, info.version).executeUpdate
//  }
  
  /**
   * Individual Steps need to fill this in. Note that it happens within an existing transaction, and
   * the higher levels deal with altering the version number in the Spaces table.
   */
  def doEvolve(info:UserInfo)(implicit conn:Connection):Unit
}
  
case class UserInfo(id:UserId, version:Int)(implicit val ecology:Ecology) extends EcologyMember {
}
object UserInfo {
  def apply(row:Row)(implicit ecology:Ecology):UserInfo = UserInfo(OID(row[Long]("id")), row[Int]("userVersion"))
}
