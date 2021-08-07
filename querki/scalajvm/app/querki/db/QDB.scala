package querki.db

import javax.inject._

import java.sql.Connection
import play.api.db.DBApi

import querki.ecology.PlayEcology
import querki.globals._

import ShardKind._

/**
 * This is the standard Querki wrapper for Play's withTransaction(). It isn't yet
 * used everywhere, but should be migrated to. Besides boilerplate reduction, it
 * automatically logs exceptions, and gives us a central place for when things change.
 * (Which has already happened once, when Play went from the static DB object to
 * requiring DI instead.)
 *
 * @author jducoeur
 */
object QDB {

  def apply[A](db: ShardKind.ShardKind)(trans: Connection => A)(implicit ecology: Ecology): A = {
    val dbapi = PlayEcology.playApi[DBApi]
    dbapi.database(dbName(db)).withTransaction { implicit conn =>
      try {
        trans(conn)
      } catch {
        case ex: Exception => {
          QLog.error("Exception executing DB transaction", ex)
          throw ex
        }
      }
    }
  }
}
