package querki.notifications

import org.scalajs.dom
import scalatags.JsDom.all._

import querki.globals._

import querki.display.Gadget

class NotifierGadget(implicit val ecology:Ecology) extends Gadget[dom.HTMLAnchorElement] with EcologyMember {
  
  lazy val Notifications = interface[Notifications]
  
  lazy val n = Notifications.numNotifications
  
  lazy val nBadge = sub(style:="font-size:x-small", n.toString)
  
  lazy val emptyIcon = i(cls:="icon-bell")
  
  lazy val fullIcon = 
    MSeq(
      i(cls:="icon-bell icon-white"),
      span(cls:="badge badge-info",
        nBadge
      )
    )
  
  lazy val top = 
    a(href:="#" /* TODO: on click, show the Notifications page */,
      if (n == 0) {
        emptyIcon
      } else {
        fullIcon
      }
    )
    
  def doRender() = {
    top
  }
}
