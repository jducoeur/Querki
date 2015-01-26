package querki.editing

import org.scalajs.dom
import org.scalajs.jquery._

import querki.globals._

import querki.api.EditFunctions._
import querki.display.input.InputGadget

/**
 * This is a very unconventional InputGadget, because we have deliberately moved away from
 * saving the entire PickList every time it changes. Instead, this hooks its child checkboxes,
 * and makes only that specific change every time one gets clicked.
 */
class PickListGadget(implicit e:Ecology) extends InputGadget[dom.HTMLUListElement](e) {
  def values = ???
  def doRender() = ???
  
  def saveCheckbox(checkbox:dom.HTMLElement) = {
    val v = $(checkbox).valueString
    val checked = $(checkbox).prop("checked").asInstanceOf[Boolean]
    val msg = 
      if (checked)
        AddToSet(path, v)
      else
        RemoveFromSet(path, v)
    saveChange(msg)
  }
  
  def hook() = {
    $(elem).find("input").change({ evt:JQueryEventObject =>
      val checkbox = evt.target.asInstanceOf[dom.HTMLElement]
      saveCheckbox(checkbox)
    })
  }

}
