package querki.evolutions.steps

import anorm._
import java.sql.Connection
import play.api.db._
import play.api.Play.current

import querki.db.ShardKind._
import querki.ecology._
import querki.evolutions._
import querki.identity.MembershipState
import querki.util.QLog
import querki.util.SqlHelpers._

class Step6 extends Step {
  val version = 6
  
  lazy val UserAccess = ecology.api[querki.identity.UserAccess]
  
  def doEvolve(info:SpaceInfo)(implicit conn:java.sql.Connection):Unit = {
    val ownerOpt = DB.withTransaction(dbName(System)) { implicit conn =>
      val stream = SQL("""
          SELECT owner
            FROM Spaces
           WHERE id = {spaceId}
          """).on("spaceId" -> info.id.raw)()
      val ownerStream = stream.map { row =>
        row.oid("owner")
      }
      ownerStream.headOption
    }
    ownerOpt match {
      case Some(ownerId) => UserAccess.addSpaceMembership(ownerId, info.id, MembershipState.owner)
      case None => QLog.error("Step6 unable to find the Owner for Space " + info.id.toThingId) 
    }
  }
}