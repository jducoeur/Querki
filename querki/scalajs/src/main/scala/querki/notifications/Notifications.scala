package querki.notifications

import querki.globals._

class NotificationsEcot(e:Ecology) extends ClientEcot(e) with Notifications {
  def implements = Set(classOf[Notifications])
  
  def numNotifications:Int = 1
}