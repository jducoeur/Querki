package querki.db

import javax.inject._

import java.sql.Connection
import play.api.db.DB
import play.api.db.{Database, DBApi}
import play.api.Play.current

import querki.ecology.PlayEcology
import querki.globals._

import ShardKind._

/**
 * This is the standard Querki wrapper for Play's DB.withTransaction. It isn't yet
 * used everywhere, but should be migrated to. Besides boilerplate reduction, it
 * automatically logs exceptions.
 * 
 * @author jducoeur
 */
object QDB {
  def apply[A](db:ShardKind.ShardKind)(trans:Connection => A)(implicit ecology:Ecology):A = {
    val dbapi = PlayEcology.playApi[DBApi]
    dbapi.database(dbName(db)).withTransaction { implicit conn =>
      try {
        trans(conn)
      } catch {
        case ex:Exception => {
          QLog.error("Exception executing DB transaction", ex)
          throw ex
        }
      }
    }
  }
}
