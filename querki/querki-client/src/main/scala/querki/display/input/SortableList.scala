package querki.display.input

import scala.scalajs.js
import org.scalajs.dom
import org.scalajs.jquery._
import scalatags.JsDom.all._

import org.querki.facades.jqueryui._

import querki.globals._

import querki.api.EditFunctions
import EditFunctions._

/**
 * TODO: all of this is way too incestuous with the fine details of how things get built
 * server-side. Now that so many smarts are here, can we have the server just describe it all
 * more conceptually, and move the details to here?
 */
class SortableListGadget(implicit e:Ecology) extends InputGadget[dom.HTMLUListElement](e)  {
  
  lazy val Gadgets = interface[querki.display.Gadgets]
  lazy val InputGadgets = interface[InputGadgets]

  def values = ???
  
  def propWrapper = $(elem).parent()
  def propIdRaw = propWrapper.dataString("propid")
  
  // The template is the model for new elements, and includes some metadata. It sits next
  // to the sortable list itself:
  lazy val template = propWrapper.find(".inputTemplate").first()
  
  def index(liElem:dom.Element) = $(liElem).data("index").asInstanceOf[Int]
  
  def numberItems() = {
    val baseItemName = template.data("basename")
    
    // Have each li know its current index, to make changes easier:
    var i = 0
    $(elem).children("li").foreach({ liElem =>
      // Assign the new index to this element:
      $(liElem).data("index", i)
      // And rewrite its path, for when its own value gets saved:
      // TODO: this rewrite is kind of horrible and brittle. Can/should we abstract out the
      // notion of "path"?
      val inputField = $(liElem).find(".list-input-element")
      inputField.attr("name", s"$baseItemName[$i]")
      
      // Now, rewrite the paths of any *child* editors under this, in case this is a Model Type:
      // TODO: this will work for first-level children, but might foul up lower-level ones.
      // Investigate what happens in the case of deeply-nested structures:
      inputField.find(".propEditor").foreach({ childElem =>
        val childjq = $(childElem)
        val childName = childjq.attr("name")
        // This is probably better done with a Regex, but I can't be arsed to figure out the
        // syntax right now:
        val rewriteKey = s"$propIdRaw["
        val rewritePreIndex = childName.indexOf(rewriteKey)
        if (rewritePreIndex != -1) {
          val rewriteIndex = rewritePreIndex + rewriteKey.length
          val rewriteEnd = childName.indexOf(']', rewriteIndex)
          val newName = childName.substring(0, rewriteIndex) + i.toString + childName.substring(rewriteEnd)
          childjq.attr("name", newName)
        }
      })
      
      i += 1
    })
  }
  
  def setupButton(buttonElem:dom.Element, icon:String, tit:String, onClick:(JQueryEventObject) => Unit) = {
    val btn = $(buttonElem)
    btn.addClass("btn")
    btn.attr("title", tit)
    btn.empty()
    btn.append(i(cls:=icon).render)
    btn.click({ evt:JQueryEventObject => onClick(evt); false })
  }
  
  def setupDeleteButton(buttonElem:dom.Element) = {
    setupButton(buttonElem, "icon-remove-sign", "Delete Item", handleDeleteListItem) 
  }
  
  def handleDeleteListItem(evt:JQueryEventObject) = {
    val targetLi = $(evt.currentTarget).parent()
    // TODO: $.get should return UndefOr[dom.Element], shouldn't it?
    val i = index(targetLi.get(0).asInstanceOf[dom.Element])
    targetLi.remove()
    numberItems()
    saveChange({ path => DeleteListItem(path, i) })
  }
    
  def handleAddListItem(evt:JQueryEventObject) = {
    // Don't pick up existing data, especially existing Gadget links:
    val newItem = template.clone(false)
    newItem.removeClass("inputTemplate")
    val delButton = button(cls:="delete-item-button btn-mini").render
    setupDeleteButton(delButton)
    // TODO: what about replaceIndexes()? Don't we need to do that every time we renumber?
    // TODO: the fact that we are creating this here, but the originals in the server, shows how
    // broken the factoring is. *ALL* of this sort of stuff belongs here.
    val newLiElem = li(span(cls:="icon-move"), newItem.get(0).asInstanceOf[dom.Element], delButton).render
    $(elem).append(newLiElem)
  	numberItems()
    // Do our best to set focus to the first relevant field of the new element:
    $(newLiElem).find(".propEditor,input,textarea").first.focus()
    Gadgets.createGadgets(newLiElem)
    InputGadgets.hookPendingGadgets()
    saveChange({ path => AddListItem(path) }).foreach { response =>
      // Important: we only try to save the new value *after* the server acks the creation of the new
      // element. Otherwise, there's a horribly easy race condition.
      if (response == PropertyChanged) {
	    val newGadgets = findGadgetsFor($(newLiElem), { frag => frag.isInstanceOf[InputGadget[_]] })
	    newGadgets.foreach { gadget =>
	      gadget.asInstanceOf[InputGadget[_]].save()
	    }          
      }
    }
  }
  
  def hook() = {
    // Hook the Add/Delete Item buttons:
    $(elem).parent().find(".add-item-button").each({ (buttonElem:dom.Element) => 
      setupButton(buttonElem, "icon-plus-sign", "Add Item", handleAddListItem) 
    }:js.ThisFunction0[dom.Element, Any])
    $(elem).parent().find(".delete-item-button").each({ (buttonElem:dom.Element) => 
      setupDeleteButton(buttonElem)
    }:js.ThisFunction0[dom.Element, Any])
    
    // Take the template out of the DOM, so that it doesn't get in the way. We mostly use it to clone
    // new elements:
    template.detach()
    
    numberItems()
    
    $(elem).sortable(SortableOptions.
      // Stop gets called after a drag-and-drop event:
      stop({ (evt:JQueryEventObject, ui:SortChangeUI) =>
        val item = ui.item.get
        val sortList = item.parent
        val oldIndex = item.data("index").asInstanceOf[Int]
        val newIndex = sortList.children("li").index(item)
        saveChange({ path => MoveListItem(path, oldIndex, newIndex) })
        numberItems()
      }:js.Function2[JQueryEventObject, SortChangeUI, Any]
    ))
  }
  
  def saveChange(mkMsg:String => PropertyChange):Future[PropertyChangeResponse] = {
    // Tell the server which element changed
    // TODO: IMPORTANT: at the moment, this is horribly susceptible to race conditions. We should
    // be suspicious about all of this until we implement history, maintain version stamps, and have
    // a clear mechanism for merging collisions. This should be sending a change *relative* to
    // a specific version of the Thing.
    val path = propWrapper.attr("name")
    saveChange(mkMsg(path))    
  }
  
  // TBD: deliberately NYI, because I'm not sure what this would mean. A SortableList necessarily has to
  // be a list *of* something.
  def doRender() = ???
}
