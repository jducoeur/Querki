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
  
  private def curSelected = {
    elemOpt.map(e => $(e).find(".active"))
  }
  lazy val selectedOption = Var[Option[JQuery]](None)
  lazy val selectedValOpt = Rx { selectedOption().map(_.valueString).filter(_.length > 0) }
  
  private def renderButtons() = {
    buttons().map { buttonInfo =>
      val clses = Seq("btn", "btn-primary") ++ (if (buttonInfo.initiallyActive) Seq("active") else Seq.empty)
      button(tpe:="button", classes(clses), value:=buttonInfo.value, buttonInfo.display)      
    }    
  }
  
  def doRender() =
    div(cls:="btn-group", data("toggle"):="buttons-radio", mods, renderButtons)
  
  def updateSelected() = { selectedOption() = curSelected }
  
  val obs = Obs(buttons, skipInitial=true) {
    $(elem).empty()
    renderButtons().map(_.render).map(opt => $(elem).append(opt))
    updateSelected()
  }
  
  override def onCreate(e:dom.HTMLDivElement) = {
    $(e).change({ evt:JQueryEventObject => updateSelected() })
    updateSelected()
  }
}
