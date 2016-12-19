package querki.editing

import org.scalajs.dom.html

import org.querki.jquery._

import querki.display.QuerkiUIUtils
import querki.display.input.InputGadget
import querki.globals._

import EditFunctions._

/**
 * The client side of the _checkList function.
 * 
 * Note that this replaces the old PickListGadget. We've deconstructed that, so this is much simpler.
 * 
 * @author jducoeur
 */
class CheckList(implicit e:Ecology) extends InputGadget[html.UList](e) with QuerkiUIUtils {
  
  lazy val Editing = interface[querki.editing.Editing]
  
  def values = ???
  def doRender() = ???
  
  lazy val propId = $(elem).tidString("prop")
  
  def saveCheckbox(checkbox:html.Element) = {
    val v = $(checkbox).valueString
    val checked = $(checkbox).prop("checked").asInstanceOf[Boolean]
    val path = Editing.propPath(propId, Some(thingId))
    val msg = 
      if (checked)
        AddToSet(path, v)
      else
        RemoveFromSet(path, v)
    saveChange(msg)
  }
  
  def hook() = {
    $(elem).find("._checkOption").change({ (evt:JQueryEventObject) =>
      val checkbox = evt.target.asInstanceOf[html.Element]
      saveCheckbox(checkbox)
    })
  }  
}
