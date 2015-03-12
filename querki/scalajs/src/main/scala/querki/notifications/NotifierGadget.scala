package querki.notifications

import org.scalajs.dom.{raw => dom}
import org.querki.jquery._
import scalatags.JsDom.all._
import rx._
import rx.ops._
import querki.globals._
import querki.display.{Gadget}
import querki.display.rx.RxTextFrag

class NotifierGadget(implicit val ecology:Ecology) extends Gadget[dom.HTMLAnchorElement] with EcologyMember {
  
  lazy val Notifications = interface[Notifications]
  
  lazy val n = Notifications.numNotifications
  lazy val nStr = n.map(_.toString)
  
  lazy val emptyIcon = i(cls:="icon-bell")
  lazy val emptyRendered = emptyIcon.render
  
  lazy val fullIcon = 
    span(
      i(cls:="icon-bell icon-white"),
      span(cls:="badge badge-info", sub(style:="font-size:x-small", new RxTextFrag(nStr)))
    )
  lazy val fullRendered = fullIcon.render
    
  // TODO: can we abstract out this general notion of contents that are reactively chosen? This is a lot
  // of boilerplate. The tricky bit is that we probably don't want to call render over and over again, just
  // *choose* reactively among components, rendering them once.
  lazy val obs = Obs(n) {
    elemOpt.foreach { e =>
      $(e).empty()
      val content = 
        if (n() == 0) {
          emptyRendered
        } else {
          fullRendered
        }
      $(e).append(content)
    }
  }
    
  override def onCreate(anchor:dom.HTMLAnchorElement) = obs
  
  def doRender() = 
    a(href:=Notifications.notificationPageUrl)
}
