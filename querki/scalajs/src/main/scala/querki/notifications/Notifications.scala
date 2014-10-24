package querki.notifications

import rx._

import querki.globals._

class NotificationsEcot(e:Ecology) extends ClientEcot(e) with Notifications {
  def implements = Set(classOf[Notifications])
  
  val numNotifications = Var(1)
}
