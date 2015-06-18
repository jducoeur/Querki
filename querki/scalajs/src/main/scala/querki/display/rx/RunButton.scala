package querki.display.rx

import org.scalajs.dom
import scalatags.JsDom.all._
import rx._
import org.querki.jquery._

import querki.display.{ButtonGadget, Gadget}
import querki.globals._

/**
 * A button for running a task that takes non-zero time.
 * 
 * This button takes two labels. The "inactive" one is what is normally shown. When the user presses
 * the button, it switches to the "active" label, disables the button, and calls the onClick callback.
 * When the onClick is finished (this usually involves some kind of asynchronous call to the server),
 * it should call done(), which sets the button back to its inactive state. 
 */
class RunButton(kind:ButtonGadget.ButtonKind, inactiveLabel:String, activeLabel:String)(onClick:RunButton => Unit) extends Gadget[dom.html.Span]  {
  private lazy val active = Var(false)

  private lazy val actualButton = 
    new ButtonGadget(
      kind,
      disabled := active,
      new RxTextFrag(Rx{if (active()) activeLabel else inactiveLabel}))(doClick)
      
  private def doClick():Unit = {
    active() = true
    onClick(this)    
  }
  
  def doRender() = span(actualButton)
  
  def done() = { active() = false }
}