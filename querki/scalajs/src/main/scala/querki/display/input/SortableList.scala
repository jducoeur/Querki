package querki.display.input

import scala.scalajs.js
import org.scalajs.dom
import org.scalajs.jquery._
import scalatags.JsDom.all._

import org.scalajs.jqueryui._

import querki.globals._

import querki.api.EditFunctions
import EditFunctions._

/**
 * TODO: all of this is way too incestuous with the fine details of how things get built
 * server-side. Now that so many smarts are here, can we have the server just describe it all
 * more conceptually, and move the details to here?
 */
class SortableListGadget(implicit e:Ecology) extends InputGadget[dom.HTMLUListElement](e)  {

  def values = ???
  
  def propWrapper = $(elem).parent()
  
  // The template is the model for new elements, and includes some metadata. It sits next
  // to the sortable list itself:
  def template = propWrapper.find(".inputTemplate").first()
  
  def numberItems() = {
    val baseItemName = template.data("basename")
    
    // Have each li know its current index, to make changes easier:
    var i = 0
    $(elem).children("li").each({ (index:js.Any, liElem:dom.Element) =>
      // Assign the new index to this element:
      $(liElem).data("index", i)
      // And rewrite its path, for when its own value gets saved:
      // TODO: this rewrite is kind of horrible and brittle. Can/should we abstract out the
      // notion of "path"?
      val inputField = $(liElem).find(".list-input-element")
      inputField.attr("name", s"$baseItemName[$i]")
      i += 1
      js.undefined
    }:js.Function2[js.Any, dom.Element, js.Any])    
  }
  
  def hook() = {    
    numberItems()
    
    $(elem).sortable(SortableOptions(
      // Stop gets called after a drag-and-drop event:
      stop = { (evt:JQueryEventObject, ui:SortChangeUI) =>
        val item = ui.item.get
        val sortList = item.parent
        val oldIndex = item.data("index").asInstanceOf[Int]
        val newIndex = sortList.children("li").index(item)
        save(oldIndex, newIndex)
        numberItems()
      }:js.Function2[JQueryEventObject, SortChangeUI, Any]
    ))
  }
  
  def save(from:Int, to:Int):Unit = {
    // Tell the server which element changed
    // TODO: IMPORTANT: at the moment, this is horribly susceptible to race conditions. We should
    // be suspicious about all of this until we implement history, maintain version stamps, and have
    // a clear mechanism for merging collisions. This should be sending a change *relative* to
    // a specific version of the Thing.
    val path = propWrapper.attr("name")
    saveChange(MoveListItem(path, from, to))
  }
  
  // TBD: deliberately NYI, because I'm not sure what this would mean. A SortableList necessarily has to
  // be a list *of* something.
  def doRender() = ???
}

object SortableListGadget {
  def apply(rawElement:dom.Element)(implicit e:Ecology) = {
    (new SortableListGadget).setElem(rawElement)
  }
}
