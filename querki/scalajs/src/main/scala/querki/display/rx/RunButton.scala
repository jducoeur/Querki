package querki.display.rx

import org.scalajs.dom
import scalatags.JsDom.all._
import rx._
import org.querki.gadgets._
import org.querki.jquery._

import querki.display.{ButtonGadget}
import querki.globals._

/**
 * A button for running a task that takes non-zero time.
 * 
 * This button takes two labels. The "inactive" one is what is normally shown. When the user presses
 * the button, it switches to the "active" label, disables the button, and calls the onClick callback.
 * When the onClick is finished (this usually involves some kind of asynchronous call to the server),
 * it should call done(), which sets the button back to its inactive state. 
 */
class RunButton(kind:ButtonGadget.ButtonKind, inactiveLabel:String, activeLabel:String, mods:Modifier*)(onClick:RunButton => Unit)(implicit val ecology:Ecology, ctx:Ctx.Owner) extends Gadget[dom.html.Span]  {
  private lazy val active = Var(false)

  private lazy val actualButton = 
    new ButtonGadget(
      kind,
      disabled := active,
      new RxTextFrag(Rx {if (active()) activeLabel else inactiveLabel}),
      mods)(doClick)
      
  private def doClick():Unit = {
    active() = true
    onClick(this)    
  }
  
  def doRender() = span(actualButton)
  
  def done() = { active() = false }
}

object RunButton {
  /**
   * This factory method is mainly here to save us from having to put annoying parens around the onClick parameter. Without
   * them, the compiler interprets that param as the body of a subclass instead of the parameter, and gets confused.
   */
  def apply(kind:ButtonGadget.ButtonKind, inactiveLabel:String, activeLabel:String, mods:Modifier*)(onClick:RunButton => Unit)(implicit ecology:Ecology) =
    new RunButton(kind, inactiveLabel, activeLabel, mods:_*)(onClick)
}
