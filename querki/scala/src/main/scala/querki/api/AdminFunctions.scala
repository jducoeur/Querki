package querki.api

import scala.concurrent.Future

/**
 * Client/Server Admin capabilities. You may only call these APIs if the logged-in session has admin rights.
 */
trait AdminFunctions {
  import AdminFunctions._
  
  /**
   * Fetch the current system statistics. This may eventually grow into a proper Dashboard, but let's
   * not over-complicate it yet.
   */
  def statistics:Future[QuerkiStats]
}

object AdminFunctions {
  case class QuerkiStats(nInvitees:Int, nFullUsers:Int, nSpaces:Int)
}
