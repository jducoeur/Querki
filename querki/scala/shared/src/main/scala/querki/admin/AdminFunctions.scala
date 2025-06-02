package querki.admin

import upickle.default.{macroRW, readwriter, ReadWriter => RW}

import scala.concurrent.Future
import querki.data.{TID, TOID}
import querki.identity.UserLevel._
import AdminFunctions.AdminUserView
import AdminFunctions.QuerkiStats

/**
 * Client/Server Admin capabilities. You may only call these APIs if the logged-in session has admin rights.
 */
trait AdminFunctions {
  import AdminFunctions._

  /**
   * Fetch the current system statistics. This may eventually grow into a proper Dashboard, but let's
   * not over-complicate it yet.
   */
  def statistics(): Future[QuerkiStats]

  /**
   * Fetch the Invitees, who still need to be upgraded to full-user status.
   */
  def pendingUsers(): Future[Seq[AdminUserView]]

  /**
   * Fetch *all* the users in the system.
   *
   * Do *not* get too attached to this! In principle, it's obviously a bad idea, and will eventually have
   * to be replaced by a search function instead.
   */
  def allUsers(): Future[Seq[AdminUserView]]

  /**
   * Upgrade the specified User to full-User status. Presumed to succeed unless it returns an Exception.
   */
  def upgradePendingUser(id: TID): Future[AdminUserView]

  /**
   * Change the given user to the given level. Note that only Superadmin can make an Admin, and nobody can make a Superadmin.
   */
  def changeUserLevel(
    id: TID,
    level: UserLevel
  ): Future[AdminUserView]

  /**
   * Get the current state of system monitoring. This is basically the very beginnings of the
   * system dashboard.
   */
  def monitor(): Future[MonitorCurrent]

  /**
   * Starts doing some profiling on the specified Space.
   */
  def beginSpaceTiming(spaceId: TOID): Future[Unit]

  /**
   * Gets the currently collection of Timed Spaces. This is *usually* empty. Note that for now it
   * is super-primitive -- just a set of OIDs. We might decide to make it fancier later, but this
   * is a rare edge-case admin feature, and I don't want to overbuild it prematurely.
   */
  def getTimedSpaces(): Future[Set[TOID]]

  /**
   * Stop profiling this Space.
   */
  def stopSpaceTiming(spaceId: TOID): Future[Unit]

  /**
   * Update the timing messages we've gotten from this Space.
   */
  def getSpaceTimingsSince(
    since: Int,
    spaceId: TOID
  ): Future[TimingMsgs]
}

object AdminFunctions {

  case class QuerkiStats(
    userCountsByLevel: Map[UserLevel, Int],
    nSpaces: Long
  )

  object QuerkiStats {
    implicit val rw: RW[QuerkiStats] = macroRW
  }

  case class AdminUserView(
    userId: TID,
    mainHandle: String,
    email: String,
    level: UserLevel
  )

  object AdminUserView {
    implicit val rw: RW[AdminUserView] = macroRW
  }

  /*
   * The following are mostly lifted directly from akka.cluster, so that we can pass the cluster's
   * state to the client without the client needing to be dependent on the Akka library.
   *
   * TODO: in upickle 0.4.4, this was being serialized with no guidance; I'm guessing that means it was a String.
   */
  sealed trait QMemberStatus { def name: String }
  case object QDown extends QMemberStatus { val name = "QDown" }
  case object QExiting extends QMemberStatus { val name = "QExiting" }
  case object QJoining extends QMemberStatus { val name = "QJoining" }
  case object QLeaving extends QMemberStatus { val name = "QLeaving" }
  case object QRemoved extends QMemberStatus { val name = "QRemoved" }
  case object QUp extends QMemberStatus { val name = "QUp" }
  case object QWeaklyUp extends QMemberStatus { val name = "QWeaklyUp" }

  object QMemberStatus {
    val items = List(QDown, QExiting, QJoining, QLeaving, QRemoved, QUp, QWeaklyUp)

    val itemsByName =
      items.map(i => (i.name -> i)).toMap

    implicit val rw: RW[QMemberStatus] = readwriter[String].bimap(_.name, itemsByName(_))
  }

  case class QMember(
    address: String,
    status: QMemberStatus
  )

  object QMember {
    implicit val rw: RW[QMember] = macroRW
  }

  case class QCurrentClusterState(
    members: Seq[QMember],
    unreachable: Seq[QMember],
    leader: String
  )

  object QCurrentClusterState {
    implicit val rw: RW[QCurrentClusterState] = macroRW
  }

  case class RunningSpace(
    name: String,
    cluster: String,
    nUsers: Int,
    size: Int,
    timestamp: Long
  )

  object RunningSpace {
    implicit val rw: RW[RunningSpace] = macroRW
  }

  case class MonitorCurrent(
    monitorNode: String,
    state: QCurrentClusterState,
    spaces: Seq[RunningSpace]
  )

  object MonitorCurrent {
    implicit val rw: RW[MonitorCurrent] = macroRW
  }

  case class TimingMsgs(
    error: Boolean,
    nowAt: Int,
    msgs: Seq[String]
  )

  object TimingMsgs {
    implicit val rw: RW[TimingMsgs] = macroRW
  }
}
