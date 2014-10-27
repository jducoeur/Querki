package querki.notifications

import scala.concurrent.Future

trait NotificationFunctions {
  /**
   * Fetch the Notifications to display for this user.
   *
   * TBD: this API bleeds the implementation detail that it needs to be a Future.
   * What's the best way to deal with this?
   */
  def getRecentNotifications():Future[Seq[NotificationInfo]]
}
