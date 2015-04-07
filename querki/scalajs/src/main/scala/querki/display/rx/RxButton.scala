package querki.display.rx

import org.scalajs.dom
import scalatags.JsDom.all._
import rx._
import org.querki.jquery._

import querki.display.{ButtonKind, ButtonGadget, Gadget}
import querki.globals._

class RxButton(kind:ButtonKind.ButtonKind, inactiveLabel:String, activeLabel:String)(onClick:RxButton => Unit) extends Gadget[dom.html.Span]  {
  private lazy val active = Var(false)
  
  private lazy val actualButton = 
    new ButtonGadget(
      kind,
      RxAttr("disabled", Rx[AttrVal]{active()}),
      new RxTextFrag(Rx{if (active()) activeLabel else inactiveLabel}))(doClick)
      
  private def doClick():Unit = {
    active() = true
    onClick(this)    
  }
  
  def doRender() = span(actualButton)
  
  def done() = { active() = false }
}