package querki.display.rx

import org.scalajs.dom
import org.scalajs.jquery._
import scalatags.JsDom.all._
import rx._
import rx.ops._

import querki.globals._

import querki.display.Gadget

case class ButtonInfo(value:String, display:String, initiallyActive:Boolean = false)

// TODO: this might get refactored together with RxSelect -- they share a lot of code. They may be an
// underlying RxSelector to be pulled out of here.
class RxButtonGroup(buttons:Rx[Seq[ButtonInfo]], mods:Modifier*) extends Gadget[dom.HTMLDivElement] {
  
  lazy val selectedValOpt = Var[Option[String]](None)
  
  private def renderButtons() = {
    buttons().map { buttonInfo =>
      val clses = Seq("btn", "btn-primary") ++ (if (buttonInfo.initiallyActive) Seq("active") else Seq.empty)
      button(tpe:="button", 
        classes(clses), 
        value:=buttonInfo.value,
        onclick:={ () => select(buttonInfo) },
        buttonInfo.display)      
    }    
  }
  
  def doRender() =
    div(cls:="btn-group", data("toggle"):="buttons-radio", mods, renderButtons)
    
  def updateSelected() = {
    buttons().find(_.initiallyActive).map(select(_))
  }
  
  def select(button:ButtonInfo) = {
    selectedValOpt() = Some(button.value)
  }
  
  val obs = Obs(buttons, skipInitial=true) {
    $(elem).empty()
    renderButtons().map(_.render).map(opt => $(elem).append(opt))
    updateSelected()
  }
  
  override def onCreate(e:dom.HTMLDivElement) = {
    updateSelected()
  }
}
