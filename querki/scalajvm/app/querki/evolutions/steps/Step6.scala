package querki.evolutions.steps

import anorm._
import anorm.SqlParser.long
import java.sql.Connection
import play.api.db._

import querki.db._
import querki.ecology._
import querki.evolutions._
import querki.identity.MembershipState
import querki.util.QLog
import querki.util.SqlHelpers._

class Step6(implicit val ecology: Ecology) extends Step {
  val version = 6

  lazy val UserAccess = ecology.api[querki.identity.UserAccess]

  def doEvolve(info: SpaceInfo)(implicit conn: java.sql.Connection): Unit = {
    val ownerOpt = QDB(ShardKind.System) { implicit conn =>
      SQL("""
          SELECT owner
            FROM Spaces
           WHERE id = {spaceId}
          """)
        .on("spaceId" -> info.id.raw)
        .as(oid("owner").singleOpt)
    }
    ownerOpt match {
      case Some(ownerId) => UserAccess.addSpaceMembership(ownerId, info.id, MembershipState.owner)
      case None          => QLog.error("Step6 unable to find the Owner for Space " + info.id.toThingId)
    }
  }
}
