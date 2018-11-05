package querki.evolutions

import anorm._
import anorm.SqlParser.{long, int}
import java.sql.Connection
import play.api.db._

import querki.ecology._

import models._

import querki.db._
import querki.db.ShardKind._
import querki.util.Config
import querki.util.SqlHelpers._

/**
 * Represents a single evolutionary step -- how to progress to this version of
 * the system.
 * 
 * By and large, a Step will specify the version, and fill in doEvolve(), which makes
 * the actual changes to the Space table.
 */
trait Step extends EcologyMember {
  /**
   * Each Step must specify this version stamp, which must be unique!
   */
  def version:Int
  
  implicit def ecology:Ecology
  
  lazy val inMemoryTest = Config.getBoolean("querki.test.inmemory", false)
  
  lazy val SpacePersistence = interface[querki.spaces.SpacePersistence]
  def SpaceSQL(spaceId:OID, query:String, version:Int = 0):SqlQuery = SpacePersistence.SpaceSQL(spaceId, query, version)
  
  def evolveUp(spaceId:OID) = {
    // TBD: is there any way to do this all within a single transaction? Since it spans DBs,
    // quite likely not, but as it stands this is a tad riskier than I like:
    val info = QDB(System) { implicit conn =>
      
      val parseRow = 
        oid("id") ~ int("version") map { case i ~ v => SpaceInfo(i, v) }
      
      SQL("""
          select * from Spaces where id = {id}
          """)
        .on("id" -> spaceId.raw)
        .as(parseRow.single)
    }
    QDB(User) { implicit spaceConn =>
      backupTables(info)(spaceConn)
      doEvolve(info)(spaceConn)        
    }
    QDB(System) { implicit conn =>
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
    // These backup operations just plain don't exist in the H2 test environment:
    if (!inMemoryTest) {
      // TODO: back up the history and attachments as well?
      SpacePersistence.SpaceSQL(info.id, """
          CREATE TABLE {bname} LIKE {tname}
          """, info.version).executeUpdate
      SpacePersistence.SpaceSQL(info.id, """
          INSERT {bname} SELECT * FROM {tname}
          """, info.version).executeUpdate
    }
  }
  
  /**
   * Individual Steps need to fill this in. Note that it happens within an existing transaction, and
   * the higher levels deal with altering the version number in the Spaces table.
   */
  def doEvolve(info:SpaceInfo)(implicit conn:Connection):Unit
}
  
case class SpaceInfo(id:OID, version:Int)(implicit val ecology:Ecology) extends EcologyMember {
  def thingTable = interface[querki.spaces.SpacePersistence].thingTable(id)
}
