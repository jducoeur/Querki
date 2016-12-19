package querki.display.rx

import org.scalajs.dom
import org.querki.jquery._
import scalatags.JsDom.all._
import rx._
import rx.ops._

import querki.globals._

case class RadioButton(v:String, label:String, mods:Modifier*)

class RxRadio(groupName:String, buttons:RadioButton*)(implicit e:Ecology) extends Gadget[dom.html.Form] {
  val ecology = e
  
  private def curSelected = {
    elemOpt.map(e => $(e).find(s"input[name=$groupName]:checked"))
  }
  
  lazy val selectedOption = Var[Option[JQuery]](None)
  lazy val selectedValOpt = Rx { selectedOption().map(_.valueString).filter(_.length > 0) }
  lazy val selectedVal = selectedValOpt.map(_.getOrElse(""))
  
  private def updateSelected() = { selectedOption() = curSelected }
  
  private class OneButton(btn:RadioButton) extends Gadget[dom.html.Input] {
    def ecology = RxRadio.this.ecology
    
    def doRender =
      input(tpe:="radio", name:=s"$groupName", id:=s"$groupName-${btn.v}", value:=btn.v)
      
    override def onCreate(e:dom.html.Input) = {
      $(e).change({ e:dom.Element => updateSelected() })
    }
  }

  def doRender = 
    form(
      cls:="radio",
      for {btn <- buttons}
        yield 
          div(cls:="form-group",
            label(
              new OneButton(btn),
              btn.label
            ),
            btn.mods
          )
    )
}