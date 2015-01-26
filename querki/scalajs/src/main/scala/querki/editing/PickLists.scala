package querki.editing

import scala.scalajs.js
import org.scalajs.dom
import org.scalajs.jquery._
import autowire._

import querki.globals._

import querki.api.EditFunctions
import EditFunctions._
import querki.display.input.InputGadget

/**
 * This is a very unconventional InputGadget, because we have deliberately moved away from
 * saving the entire PickList every time it changes. Instead, this hooks its child checkboxes,
 * and makes only that specific change every time one gets clicked.
 */
class PickListGadget(implicit e:Ecology) extends InputGadget[dom.HTMLUListElement](e) {
  
  lazy val Editing = interface[Editing]
  lazy val PageManager = interface[querki.display.PageManager]
  
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
    $(elem).find("._pickOption").change({ evt:JQueryEventObject =>
      val checkbox = evt.target.asInstanceOf[dom.HTMLElement]
      saveCheckbox(checkbox)
    })
    
    $(elem).find("._quickCreator").each({ e:dom.Element => hookCreator(e) }:js.ThisFunction0[dom.Element, Any])
  }
  
  /**
   * Create a new element on the Pick List. This is only present if the QL specifies withAdd.
   */
  def quickCreate(evt:JQueryEventObject, createNameQ:JQuery) = {
    evt.stopPropagation()
    
    val createModel = TID(createNameQ.dataString("model"))
    val createProp = TID(createNameQ.dataString("propid"))
    val createVal = createNameQ.valueString
    
    val msg = ChangePropertyValue(Editing.propPath(createProp), List(createVal))
    
    for {
      thingInfo <- Client[EditFunctions].create(createModel, Seq(msg)).call()
      addResponse <- saveChange(AddToSet(path, thingInfo.oid.underlying))
      reloadedPage <- PageManager.reload()
    }
      StatusLine.showBriefly("Saved")              
  }

  def hookCreator(creator:dom.Element) = {
    val createNameQ = $(creator).find("._quickCreateProp")
    
    // Intercept the return key:
    createNameQ.change({ evt:JQueryEventObject => evt.stopPropagation() })
    createNameQ.keydown({ evt:JQueryEventObject =>
      if (evt.which == 13) {
        quickCreate(evt, createNameQ)
        false
      } else
        true
    })
    
    // Or when they press the Add button:
    $(creator).find("._quickCreate").click({ evt:JQueryEventObject => quickCreate(evt, createNameQ) })
  }
}
