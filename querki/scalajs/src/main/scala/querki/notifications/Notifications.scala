package querki.notifications

import rx._

import querki.globals._

import querki.comm._
import querki.pages.{Page, PageFactory, ParamMap}
import querki.util.{Contributor, Publisher}

class NotificationsEcot(e:Ecology) extends ClientEcot(e) with Notifications {
  
  def implements = Set(classOf[Notifications])

  lazy val PageManager = interface[querki.display.PageManager]
  lazy val Pages = interface[querki.pages.Pages]
  lazy val controllers = interface[querki.comm.ApiComm].controllers
  
  val numNotifications = Var(1)
  
  /**
   * After we load each Page, check with the server about how many Notifications there currently are.
   */
  override def postInit() = {
    PageManager.afterPageLoads += new Contributor[Page,Unit] {
      def notify(evt:Page, sender:Publisher[Page, Unit]) = {
	    controllers.NotificationController.numNotifications().callAjax().foreach { nStr =>
	      // Update any reactive listeners:
	      numNotifications() = nStr.toInt
	    }        
      }
    }
    
    Pages.registerStandardFactory("_notifications", { (params) => new NotificationsPage(params) })
  }
}
