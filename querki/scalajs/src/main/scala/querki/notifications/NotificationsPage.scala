package querki.notifications

import scalatags.JsDom.all.{input => inp, _}

import autowire._

import moment._

import querki.globals._

import querki.display.QText
import querki.pages.{Page, PageContents, ParamMap}

class NotificationsPage(params:ParamMap)(implicit e:Ecology) extends Page(e) with EcologyMember  {

  lazy val Client = interface[querki.client.Client]
  lazy val Notifications = interface[Notifications]
  
  def pageContent = {
    Client[NotificationFunctions].getRecentNotifications().call().map { notifications =>
      val maxNote = notifications.map(_.id).max
      Client[NotificationFunctions].readThrough(maxNote).call().foreach { dummy =>
        // Once we tell the server to update, refresh things.
        // TODO: eventually, this should become reactively automatic.
        Notifications.checkNotifications()
      }
      val guts = 
        div(
          h1("Recent Messages"),
          p("""NOTE: yes, we will be replacing this page with a fancy popup notifications pane, a la Facebook and Google+, 
              |when we have time to do so. Please bear with us for the time being.""".stripMargin),
          div(cls:="notePage",
          for (note <- notifications)
            yield MSeq(
              hr,
              p("From ", span(cls:="noteSender", note.sender.name), " ", span(cls:="noteTime", moment(note.sentTime).calendar())),
              new QText(note.rendered.headline, cls:="noteHeadline"),
              new QText(note.rendered.content, cls:="noteContent")
            )
        ))
      PageContents(s"Recent Messages", guts)
    }
  }
}
