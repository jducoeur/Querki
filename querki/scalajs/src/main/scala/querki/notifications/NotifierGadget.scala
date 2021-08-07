package querki.notifications

import org.scalajs.dom.{raw => dom}
import org.querki.gadgets._
import org.querki.jquery._
import scalatags.JsDom.all._
import rx._
import querki.globals._

class NotifierGadget(
  implicit
  val ecology: Ecology,
  ctx: Ctx.Owner
) extends Gadget[dom.HTMLAnchorElement]
     with EcologyMember {

  lazy val Notifications = interface[Notifications]

  lazy val n = Notifications.numNotifications
  lazy val nStr = n.map(_.toString)

  lazy val emptyIcon = i(cls := "glyphicon glyphicon-bell notifier-empty")
  lazy val emptyRendered = emptyIcon.render

  lazy val fullIcon =
    span(
      i(cls := "glyphicon glyphicon-bell notifier-full"),
      span(cls := "badge badge-info", new RxTextFrag(nStr))
    )
  lazy val fullRendered = fullIcon.render

  // TODO: can we abstract out this general notion of contents that are reactively chosen? This is a lot
  // of boilerplate. The tricky bit is that we probably don't want to call render over and over again, just
  // *choose* reactively among components, rendering them once.
  lazy val obs = n.trigger {
    elemOpt.foreach { e =>
      $(e).empty()
      val content =
        if (n.now == 0) {
          emptyRendered
        } else {
          fullRendered
        }
      $(e).append(content)
    }
  }

  override def onCreate(anchor: dom.HTMLAnchorElement) = obs

  def doRender() =
    a(href := Notifications.notificationPageUrl)
}
