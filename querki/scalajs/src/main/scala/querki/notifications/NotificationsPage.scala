package querki.notifications

import scalatags.JsDom.all.{input => inp, _}

import autowire._

import querki.globals._

import querki.display.QText
import querki.pages.{Page, PageContents, ParamMap}

class NotificationsPage(params:ParamMap)(implicit e:Ecology) extends Page(e) with EcologyMember  {

  lazy val Client = interface[querki.client.Client]
  
  def pageContent = {
    Client[NotificationFunctions].getRecentNotifications().call().map { notifications =>
      val guts = 
        div(
          h1("Recent Messages"),
          p("""NOTE: yes, we will be replacing this page with a fancy popup notifications pane, a la Facebook and Google+, 
              |when we have time to do so. Please bear with us for the time being.""".stripMargin),
          div(cls:="notePage",
          for (note <- notifications)
            yield MSeq(
              hr,
              p("From ", span(cls:="noteSender", note.sender.name), " ", span(cls:="noteTime", "PLACE TIME HERE")),
              new QText(note.rendered.headline, cls:="noteHeadline"),
              new QText(note.rendered.content, cls:="noteContent")
            )
        ))
      PageContents(s"Recent Messages", guts)
    }
  }
}
