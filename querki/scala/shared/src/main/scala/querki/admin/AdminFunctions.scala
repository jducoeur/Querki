package querki.admin

import scala.concurrent.Future
import querki.data.TID
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
  def statistics():Future[QuerkiStats]
  
  /**
   * Fetch the Invitees, who still need to be upgraded to full-user status.
   */
  def pendingUsers():Future[Seq[AdminUserView]]
  
  /**
   * Fetch *all* the users in the system.
   *
   * Do *not* get too attached to this! In principle, it's obviously a bad idea, and will eventually have
   * to be replaced by a search function instead.
   */
  def allUsers():Future[Seq[AdminUserView]]
  
  /**
   * Upgrade the specified User to full-User status. Presumed to succeed unless it returns an Exception.
   */
  def upgradePendingUser(id:TID):Future[AdminUserView]
  
  /**
   * Change the given user to the given level. Note that only Superadmin can make an Admin, and nobody can make a Superadmin.
   */
  def changeUserLevel(id:TID, level:UserLevel):Future[AdminUserView]
  
  /**
   * Get the current state of system monitoring. This is basically the very beginnings of the
   * system dashboard.
   */
  def monitor():Future[MonitorCurrent]
}

object AdminFunctions {
  case class QuerkiStats(userCountsByLevel:Map[UserLevel, Int], nSpaces:Long)
  case class AdminUserView(userId:TID, mainHandle:String, email:String, level:UserLevel)
  
  /*
   * The following are mostly lifted directly from akka.cluster, so that we can pass the cluster's
   * state to the client without the client needing to be dependent on the Akka library.
   * 
   * TODO: is there a better way to do this?
   */
  sealed trait QMemberStatus
  case object QDown extends QMemberStatus
  case object QExiting extends QMemberStatus
  case object QJoining extends QMemberStatus
  case object QLeaving extends QMemberStatus
  case object QRemoved extends QMemberStatus
  case object QUp extends QMemberStatus
  
  case class QMember(address:String, status:QMemberStatus)
  case class QCurrentClusterState(members:Seq[QMember], unreachable:Seq[QMember], leader:String)
  
  case class RunningSpace(name:String, cluster:String, nUsers:Int, size:Int, timestamp:Long)
  case class MonitorCurrent(monitorNode:String, state:QCurrentClusterState, spaces:Seq[RunningSpace])
}
