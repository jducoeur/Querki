package querki.notifications

import scalatags.JsDom.all.{input => inp, _}

import autowire._

import querki.globals._

import querki.pages.{Page, PageContents, ParamMap}

class NotificationsPage(params:ParamMap)(implicit e:Ecology) extends Page(e) with EcologyMember  {

  lazy val Client = interface[querki.client.Client]
  
  def pageContent = {
    Client[NotificationFunctions].getRecentNotifications().call().map { notifications =>
      val guts = div("Notifications will go here")
      PageContents(s"Notifications for YOUR NAME HERE", guts)
    }
  }
}
