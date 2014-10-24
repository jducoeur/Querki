package querki.notifications

import org.scalajs.dom
import scalatags.JsDom.all._
import rx._
import rx.ops._

import querki.globals._

import querki.display.Gadget

/**
 * A reactive text Modifier, which lets you place some Text into Scalatags, and have it change
 * when the underlying Rx changes. It does this by setting the .text() jQuery property of the
 * parent node.
 * 
 * Note that this requires an Rx[String] specifically, to keep things simple. Use rx.ops.map to
 * turn other types into Strings.
 * 
 * TODO: this shares enough similarity with Gadget that they should probably get merged. The
 * tracking of parent probably can and should become a standard part of Gadget.
 */
class RxTextFrag(rx:Rx[String]) extends scalatags.jsdom.Frag {
  
  var _elem:Option[dom.Text] = None
  def elemOpt = _elem
  
  lazy val obs = Obs(rx) {
    parentOpt.foreach { parent =>
      $(parent).text(rx())
    }
  }
  
  def onCreate() = obs
    
  def render = {
    val result = dom.document.createTextNode("")
    _elem = Some(result)
    onCreate()
    result
  }
  
  var parentOpt:Option[dom.Element] = None
  
  override def applyTo(parent:dom.Element) = {
    parentOpt = Some(parent)
    super.applyTo(parent)
  }
}

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
