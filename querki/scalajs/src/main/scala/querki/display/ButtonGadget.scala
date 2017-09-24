package querki.display

import org.scalajs.dom.{raw => dom}
import org.querki.gadgets._
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
class ButtonGadget(kind:ButtonKind, mods:Modifier*)(onClick:() => Unit)(implicit val ecology:Ecology) extends Gadget[dom.HTMLAnchorElement] {
  // HACK: we really need a way to make buttons more composable. This should include the querkiButton and iconButton functions as well.
  val addlCls:String = ""
  
  def doRender() = {
    a(cls:=s"btn $kind $addlCls", mods)
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

class SmallButtonGadget(kind:ButtonKind, mods:Modifier*)(onClick:() => Unit)(implicit e:Ecology) extends ButtonGadget(kind, mods)(onClick)
{
  override val addlCls:String = "btn-xs"
}
