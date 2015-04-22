package querki.notifications

import scala.concurrent.Future

trait NotificationFunctionsEmpty extends NotificationFunctions {
  def getRecentNotifications():Future[Seq[NotificationInfo]] = ???
  def numNewNotifications():Int = ???
  def readThrough(id:Common.NotificationId):Unit = ???
}
