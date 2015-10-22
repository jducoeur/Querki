package querki.apps

import anorm.{Success=>AnormSuccess,_}

import play.api.db._
import play.api.Play.current

import models.{OID}

import querki.db._
import ShardKind._
import querki.globals._

private [apps] trait AppsPersistence extends EcologyInterface {
  /**
   * Returns the immediate ancestors of this Space, in order.
   * 
   * LONG RUNNING -- this does a DB transaction, so do not use it casually!
   */
  def lookupApps(space:OID):Seq[OID]
  
  /**
   * Adds the specified App, to the end of the given Space's App chain.
   */
  def addApp(spaceId:OID, appId:OID):Unit
}

/**
 * The actual DB layer for Apps. Split out from AppsEcot so that we can stub it for testing.
 * 
 * @author jducoeur
 */
class AppsPersistenceEcot(e:Ecology) extends QuerkiEcot(e) with AppsPersistence {
  def lookupApps(space:OID):Seq[OID] = {
    DB.withTransaction(dbName(ShardKind.System)) { implicit conn =>
      val appRows = SQL("""
        SELECT * FROM Apps
         WHERE space_id = {spaceId}
      ORDER BY position""").on("spaceId" -> space.raw)()
      appRows map { row =>
        OID(row[Long]("app_id"))
      } force
    }
  }
  
  def addApp(spaceId:OID, appId:OID):Unit = {
    DB.withTransaction(dbName(ShardKind.System)) { implicit conn =>
      val appRows = SQL("""
        SELECT * FROM Apps
         WHERE space_id = {spaceId}
      ORDER BY position""").on("spaceId" -> spaceId.raw)()
      val appIds = appRows map { row =>
        OID(row[Long]("app_id"))
      }
      // Sanity-check that this App isn't already in the list
      if (!appIds.contains(appId)) {
        val nApps = appIds.length
        SQL("""
          INSERT INTO Apps
          (space_id,  app_id, app_version, position) VALUES
          ({spaceId}, {appId}, 0,          {position})
          """).on(
            "spaceId" -> spaceId.raw,
            "appId" -> appId.raw,
            "position" -> (nApps + 1)
          ).executeUpdate()
      }
    }
  }
}
