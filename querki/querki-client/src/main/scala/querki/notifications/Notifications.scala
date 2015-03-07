package querki.notifications

import autowire._
import rx._

import querki.globals._

import querki.comm._
import querki.pages.{Page, PageFactory, ParamMap}
import querki.util.{Contributor, Publisher}

class NotificationsEcot(e:Ecology) extends ClientEcot(e) with Notifications {
  
  def implements = Set(classOf[Notifications])

  lazy val Client = interface[querki.client.Client]
  lazy val PageManager = interface[querki.display.PageManager]
  lazy val Pages = interface[querki.pages.Pages]
  
  val numNotifications = Var(0)
  
  val pageName = "_notifications"
  
  def notificationPageUrl:URL = PageManager.pageUrl(pageName)
  
  def checkNotifications() = {
    Client[NotificationFunctions].numNewNotifications().call().map { num =>
      // Update any reactive listeners:
      numNotifications() = num
    }            
  }
  
  /**
   * After we load each Page, check with the server about how many Notifications there currently are.
   * 
   * TODO: once we have a true bidirectional connection (WebSockets or such), this should be hooked
   * into that.
   */
  override def postInit() = {
    PageManager.afterPageLoads += new Contributor[Page,Unit] {
      def notify(evt:Page, sender:Publisher[Page, Unit]) = {
        checkNotifications()
      }
    }
    
    Pages.registerStandardFactory(pageName, { (params) => new NotificationsPage(params) })
  }
}
