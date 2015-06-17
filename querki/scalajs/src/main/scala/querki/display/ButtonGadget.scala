package querki.display

import org.scalajs.dom.{raw => dom}
import org.querki.jquery._
import scalatags.JsDom.all._

import querki.globals._

object ButtonGadget {
  type ButtonKind = Int
  
  val Normal:ButtonKind = 1
  val Info:ButtonKind = 2
  val Primary:ButtonKind = 3
}
import ButtonGadget._

/**
 * A simple Gadget that encapsulates the notion of a Button that does something when clicked. Almost
 * trivial, but this saves some boilerplate.
 * 
 * NOTE: if you want a stateful button -- one that changes label and disables itself until completed --
 * then use RxButton instead. By and large, err on the side of using that.
 */
class ButtonGadget(kind:ButtonKind, mods:Modifier*)(onClick:() => Unit) extends Gadget[dom.HTMLAnchorElement] {
  def doRender() = {
    val kindStr = kind match {
      case Normal => "btn-default"
      case Info => "btn-info"
      case Primary => "btn-primary"
    }
    a(cls:=s"btn $kindStr", mods)
  }
  
  override def onCreate(e:dom.HTMLAnchorElement) = {
    $(elem).click({ evt:JQueryEventObject =>
      onClick()
    })
  }
}
