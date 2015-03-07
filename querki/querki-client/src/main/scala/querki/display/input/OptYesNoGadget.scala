package querki.display.input

import scala.scalajs.js
import org.scalajs.dom
import org.scalajs.jquery._

import querki.globals._

import querki.api.EditFunctions
import EditFunctions._

/**
 * Represents the standard control for an Optional YesNo input.
 * 
 * TODO: this is far more specialized than it should be -- it should get generalized.
 * TODO: stop relying on finding the _optYesNo class: find this based on Collection and Type instead.
 * TODO: render this server-side, not client-side!
 */
class OptYesNoGadget(implicit e:Ecology) extends InputGadget[dom.HTMLSpanElement](e) {
  // For the moment, we bypass this mechanism and call saveChange directly:
  def values = ???
  
  def hook() = {
    $(elem).find("button").click({ btn:dom.Element =>
      val btnElem = $(btn)
      saveChange(ChangePropertyValue(btnElem.attr("name"), List(btnElem.valueString)))
    }:js.ThisFunction0[dom.Element, Any])
  }
  
  def doRender() = ???
}
