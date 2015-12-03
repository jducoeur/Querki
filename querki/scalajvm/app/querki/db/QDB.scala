package querki.db

import java.sql.Connection
import play.api.db.DB
import play.api.Play.current

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
  def apply[A](db:ShardKind.ShardKind)(trans:Connection => A):A = {
    DB.withTransaction(dbName(db)) { implicit conn =>
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