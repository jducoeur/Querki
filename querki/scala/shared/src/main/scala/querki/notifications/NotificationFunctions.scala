package querki.notifications

import scala.concurrent.Future

trait NotificationFunctions {

  /**
   * Fetch the Notifications to display for this user.
   *
   * TBD: this API bleeds the implementation detail that it needs to be a Future.
   * What's the best way to deal with this?
   */
  def getRecentNotifications(): Future[Seq[NotificationInfo]]

  /**
   * Fetch the number of unread Notifications for this user.
   */
  def numNewNotifications(): Int

  /**
   * Declares the the user has read through the specified Notification.
   */
  def readThrough(id: Common.NotificationId): Unit
}
