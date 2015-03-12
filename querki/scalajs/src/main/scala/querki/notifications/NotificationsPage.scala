package querki.notifications

import scala.scalajs.js
import org.scalajs.dom.{raw => dom}
import org.querki.jquery._
import scalatags.JsDom.all.{input => inp, _}

import autowire._

import org.querki.facades.moment._

import querki.globals._

import querki.display.QText
import querki.pages.{Page, PageContents, ParamMap}

class NotificationsPage(params:ParamMap)(implicit e:Ecology) extends Page(e) with EcologyMember  {

  lazy val Client = interface[querki.client.Client]
  lazy val Notifications = interface[Notifications]
  
  /**
   * HORRIBLE HACK: the comment, as rendered by the server's CommentNotifier, includes a terrible
   * hard-coded link. The link's format has an embedded # to go to the actual comment. That, of course,
   * breaks in the new client. So we rewrite it to be something we understand.
   * 
   * For now, this is assuming that we only have comment notifications. That's not quite right, but
   * should do for the very short term.
   * 
   * TODO: this whole approach is fundamentally wrong. Redo comment rendering so that the information
   * sent to the client includes all the necessaries (remembering that Notifications can be in other
   * Spaces), and construct the link Client-side.
   */
  override def beforeRender() = {
    renderedContentFuture.foreach { thisPage =>
      // WTF? Why is this JQExt conversion needed? For some reason, the usual implicit isn't working?
      $(elem).find(".noteHeadline a").foreach({ rawElem:dom.Element =>
        val anchor = rawElem.asInstanceOf[dom.HTMLAnchorElement]
        val href = $(anchor).attr("href").get
        val adjusted = href.replace("#comment", "?showComment=comment")
        $(anchor).attr("href", adjusted)
      })
    }
  }
  
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
