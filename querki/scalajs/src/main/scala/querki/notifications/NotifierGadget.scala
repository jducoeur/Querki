package querki.notifications

import org.scalajs.dom
import scalatags.JsDom.all._
import rx._

import querki.globals._

import querki.display.Gadget

class NotifierGadget(implicit val ecology:Ecology) extends Gadget[dom.HTMLAnchorElement] with EcologyMember {
  
  lazy val Notifications = interface[Notifications]
  
  lazy val n = Notifications.numNotifications
  
  // TODO: this is fugly. Can we come up with a general concept of a Gadget whose Text is a Reactive, or
  // better yet a pure-Reactive Text Modifier?
  class BadgeGadget extends Gadget[dom.HTMLElement] {  
    lazy val nObs = Obs(n) {
      elemOpt.foreach { elem =>
        $(elem).text(n().toString)
      }
    }
    
    override def onCreate(elem:dom.HTMLElement) = {
      // Kick the reactive into gear:
      nObs
    }
    
    def doRender() = sub(style:="font-size:x-small")
  }
  
  lazy val emptyIcon = i(cls:="icon-bell")
  
  lazy val fullIcon = 
    MSeq(
      i(cls:="icon-bell icon-white"),
      span(cls:="badge badge-info", new BadgeGadget)
    )
  
  lazy val top = 
    a(href:="#" /* TODO: on click, show the Notifications page */,
      // TODO: this should be reactive:
      if (n() == 0) {
        emptyIcon
      } else {
        fullIcon
      }
    )
    
  def doRender() = {
    top
  }
}
