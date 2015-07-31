package querki.display

import org.scalajs.dom.{raw => dom}
import org.querki.jquery._
import scalatags.JsDom.all._

import querki.globals._

object ButtonGadget {
  type ButtonKind = String
  
  val Normal:ButtonKind = "btn-default"
  val Info:ButtonKind = "btn-info"
  val Primary:ButtonKind = "btn-primary"
  val Warning:ButtonKind = "btn-warning"
  val Danger:ButtonKind = "btn-danger"
}
import ButtonGadget._

/**
 * A simple Gadget that encapsulates the notion of a Button that does something when clicked. Almost
 * trivial, but this saves some boilerplate.
 * 
 * NOTE: if you want a stateful button -- one that changes label and disables itself until completed --
 * then use RunButton instead. By and large, err on the side of using that.
 */
class ButtonGadget(kind:ButtonKind, mods:Modifier*)(onClick:() => Unit) extends Gadget[dom.HTMLAnchorElement] {
  def doRender() = {
    a(cls:=s"btn $kind", mods)
  }
  
  override def onCreate(e:dom.HTMLAnchorElement) = {
    $(elem).click({ evt:JQueryEventObject =>
      onClick()
    })
    $(elem).keydown { (evt:JQueryEventObject) =>
      if (evt.which == 13) {
        onClick();
        false
      }  
    }
  }
}
