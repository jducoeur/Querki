package querki.evolutions

import anorm._
import java.sql.Connection
import play.api.db._
import play.api.Play.current

import models._
import models.Space.SpaceSQL

import querki.db.ShardKind._

/**
 * Represents a single evolutionary step -- how to progress to this version of
 * the system.
 * 
 * By and large, a Step will specify the version, and fill in doEvolve(), which makes
 * the actual changes to the Space table.
 */
trait Step {
  /**
   * Each Step must specify this version stamp, which must be unique!
   */
  def version:Int
  
  def evolveUp(spaceId:OID) = {
    // TBD: is there any way to do this all within a single transaction? Since it spans DBs,
    // quite likely not, but as it stands this is a tad riskier than I like:
    val info = DB.withTransaction(dbName(System)) { implicit conn =>
      val row = SQL("""
          select * from Spaces where id = {id}
          """).on("id" -> spaceId.raw).apply().headOption.get
      SpaceInfo(row)
    }
    DB.withTransaction(dbName(User)) { implicit spaceConn =>
      backupTables(info)(spaceConn)
      doEvolve(info)(spaceConn)        
    }
    DB.withTransaction(dbName(System)) { implicit conn =>
      SQL("""
          UPDATE Spaces SET version = {version} WHERE id = {id}
          """).on("version" -> version, "id" -> spaceId.raw).executeUpdate
    }
  }
  
  /**
   * For the time being at least, we back up the current state of the Space before we
   * evolve it. Eventually, we'll want to get rid of these, but better safe than sorry
   * for now.
   */
  def backupTables(info:SpaceInfo)(implicit conn:Connection):Unit = {
    // TODO: back up the history and attachments as well?
    SpaceSQL(info.id, """
        CREATE TABLE {bname} LIKE {tname}
        """, info.version).executeUpdate
    SpaceSQL(info.id, """
        INSERT {bname} SELECT * FROM {tname}
        """, info.version).executeUpdate
  }
  
  /**
   * Individual Steps need to fill this in. Note that it happens within an existing transaction, and
   * the higher levels deal with altering the version number in the Spaces table.
   */
  def doEvolve(info:SpaceInfo)(implicit conn:Connection):Unit
}
  
case class SpaceInfo(id:OID, version:Int) {
  def thingTable = Space.thingTable(id)
}
object SpaceInfo {
  def apply(row:SqlRow):SpaceInfo = SpaceInfo(OID(row.get[Long]("id").get), row.get[Int]("version").get)
}
