package querki.api

import scala.concurrent.Future

import querki.data.TID
import querki.identity.UserLevel._

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
   * Upgrade the specified User to full-User status. Presumed to succeed unless it returns an Exception.
   */
  def upgradePendingUser(id:TID):Future[Unit]
}

object AdminFunctions {
  case class QuerkiStats(userCountsByLevel:Map[UserLevel, Int], nSpaces:Long)
  case class AdminUserView(userId:TID, mainHandle:String, email:String, level:UserLevel)
}
