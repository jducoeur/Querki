package querki.notifications

import rx._

import querki.globals._

import querki.comm._

class NotificationsEcot(e:Ecology) extends ClientEcot(e) with Notifications {
  
  def implements = Set(classOf[Notifications])
  
  lazy val controllers = interface[querki.comm.ApiComm].controllers
  
  val numNotifications = Var(1)
  
  /**
   * TODO: we really should update this each time we load a Page:
   */
  override def postInit() = {
    controllers.NotificationController.numNotifications().callAjax().foreach { nStr =>
      numNotifications() = nStr.toInt
    }
  }
}
