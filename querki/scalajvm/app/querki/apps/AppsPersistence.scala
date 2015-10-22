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
}

/**
 * The actual DB layer for Apps. Split out from AppsEcot so that we can stub it for testing.
 * 
 * @author jducoeur
 */
class AppsPersistenceEcot(e:Ecology) extends QuerkiEcot(e) {
  def lookupApps(space:OID):Seq[OID] = {
    DB.withTransaction(dbName(ShardKind.User)) { implicit conn =>
      val appRows = SQL("""
        SELECT * FROM Apps
         WHERE space_id = {spaceId}
      ORDER BY position""").on("spaceId" -> space.raw)()
      appRows map { row =>
        OID(row[Long]("app_id"))
      }
    }
  }
}
