package querki.notifications

import org.scalajs.dom
import scalatags.JsDom.all._
import rx._
import rx.ops._

import querki.globals._

import querki.display.{Gadget, RxTextFrag}

class NotifierGadget(implicit val ecology:Ecology) extends Gadget[dom.HTMLAnchorElement] with EcologyMember {
  
  lazy val Notifications = interface[Notifications]
  
  lazy val n = Notifications.numNotifications
  lazy val nStr = n.map(_.toString)
  
  lazy val emptyIcon = i(cls:="icon-bell")
  
  lazy val fullIcon = 
    MSeq(
      i(cls:="icon-bell icon-white"),
      span(cls:="badge badge-info", sub(style:="font-size:x-small", new RxTextFrag(nStr)))
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
