package querki.display.rx

import org.scalajs.dom.{raw => dom}
import org.querki.jquery._
import scalatags.JsDom.all._
import rx._
import rx.ops._
import org.querki.gadgets._

import querki.globals._

case class ButtonInfo(value:String, display:String, initiallyActive:Boolean, labelMods:Modifier*)

// TODO: this might get refactored together with RxSelect -- they share a lot of code. They may be an
// underlying RxSelector to be pulled out of here.
class RxButtonGroup(buttons:Rx[Seq[ButtonInfo]], mods:Modifier*)(implicit val ecology:Ecology) 
  extends Gadget[dom.HTMLDivElement] with querki.display.QuerkiUIUtils
{
  
  lazy val selectedValOpt = Var[Option[String]](None)
  lazy val selectedTIDOpt = selectedValOpt.map(_.map(TID(_)))
  
  private def renderButtons() = {
    buttons().map { buttonInfo =>
      val clses = Seq("btn", "btn-primary") ++ (if (buttonInfo.initiallyActive) Seq("active") else Seq.empty)
      label(classes(clses),
        input(
          tpe:="radio",
          if (buttonInfo.initiallyActive) checked:="checked",
          value:=buttonInfo.value),
        onclick:={ () => spew(s"Setting buttons to $buttonInfo"); select(buttonInfo) },
        buttonInfo.display,
        buttonInfo.labelMods)
    }    
  }
  
  def doRender() =
    div(cls:="btn-group", data("toggle"):="buttons", mods, renderButtons)
    
  def updateSelected() = {
    buttons().find(_.initiallyActive).map(select(_))
  }
  
  // This is private because it doesn't actually change *to* this button, which is what outside
  // code would expect. Use choose() instead:
  private def select(button:ButtonInfo) = {
    selectedValOpt() = Some(button.value)
  }
  
  // Change to the specified button; we do this by pseudo-clicking on it, since that's what makes
  // Bootstrap happiest:
  def choose(button:ButtonInfo) = {
    $(elem)
      .find("input")
      .filter { inp:dom.Element => $(inp).valueString == button.value }
      .parent().click()
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
