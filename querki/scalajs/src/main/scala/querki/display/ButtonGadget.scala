package querki.display

import org.scalajs.dom.{raw => dom}
import org.querki.jquery._
import scalatags.JsDom.all._

import querki.globals._

object ButtonKind extends Enumeration {
  type ButtonKind = Value
  
  val Normal, Info, Primary = Value
}
import ButtonKind._

/**
 * A simple Gadget that encapsulates the notion of a Button that does something when clicked. Almost
 * trivial, but this saves some boilerplate.
 */
class ButtonGadget(kind:ButtonKind, mods:Modifier*)(onClick: => Unit) extends Gadget[dom.HTMLAnchorElement] {
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
      onClick
    })
  }
}
