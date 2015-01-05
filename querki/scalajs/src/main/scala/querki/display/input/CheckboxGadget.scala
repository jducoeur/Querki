package querki.display.input

import scala.scalajs.js
import js.UndefOr
import org.scalajs.dom
import org.scalajs.jquery._

import querki.globals._

class CheckboxGadget(implicit e:Ecology) extends InputGadget[dom.HTMLInputElement](e)  {
  def values = {
    if ($(elem).prop("checked").asInstanceOf[Boolean])
      List("on")
    else
      List("off")
  }
  
  def hook() = {
    $(elem).change({ event:JQueryEventObject => 
      save() 
    })
  }
  
  def doRender() = ???
}
