package querki.editing

import scala.scalajs.js
import org.scalajs.dom.{raw => dom}
import org.querki.jquery._
import org.querki.facades.jqueryui._
import scalatags.JsDom.all._
import autowire._
import rx._

import querki.globals._

import EditFunctions.{FullEditInfo, PropEditInfo}
import querki.data.{BasicThingInfo}
import querki.display.input.InputGadget

class PropertySection(val page:ModelDesignerPage, nam:String, props:Seq[PropEditInfo], val thing:BasicThingInfo, 
  val editInfo:FullEditInfo, sortable:Boolean = true)(implicit val e:Ecology) 
  extends InputGadget[dom.HTMLUListElement](e) 
{
  lazy val Gadgets = interface[querki.display.Gadgets]
  lazy val PageManager = interface[querki.display.PageManager]
  
  val tid = thing.urlName
  
  /**
   * The Properties currently in this section. Note that this is a var so that more props can be added.
   */
  val propIds = Var(props.map(_.propInfo.oid).toSet)
  
  // Note that this is only ever invoked on the Instance Property Section:
  def values = {
    $(elem).children("li").map({ propElem:dom.Element =>
      $(propElem).data("propid")
    }:js.ThisFunction0[dom.Element, Any]).get().asInstanceOf[js.Array[String]]
  }

  // Note that this is only ever invoked on the Instance Property Section:
  def onMoved() = {
    if (sortable) {
      save()
      page.reindex()
    }
  }
  
  def hook() = {
    if (sortable) {
      $(elem).sortable(SortableOptions.
        // That is, the two PropertySections are linked, and you can drag between them:
        connectWith("._propertySection").
        // Stop gets called after a drag-and-drop event:
        stop({ (evt:JQueryEventObject, ui:SortChangeUI) =>
          val item = ui.item.get
          // IMPORTANT: note that we save the Instance Props whenever there is a drop, but this
          // stop event may be coming from Model Props if the user has dragged across the boundary.
          page.instancePropSection().onMoved()
        }:js.Function2[JQueryEventObject, SortChangeUI, Any]
      ))
    }
  }
  
  def appendEditor(editInfo:PropEditInfo, openEditor:Boolean) = {
    val editor = new PropValueEditor(editInfo, this, openEditor)
    $(elem).append(editor.rendered)
    propIds() += editInfo.propInfo.oid
    onMoved()
  }
  
  def removeEditor(editor:PropValueEditor) = {
    val child = $(editor.elem)
    child.hide(400, { () => 
      child.remove() 
      propIds() -= editor.info.propInfo.oid
      page.instancePropSection().onMoved()
    })
  }
  
  def refreshEditor(editor:PropValueEditor)(after: => Unit) = {
    Client[EditFunctions].getOnePropertyEditor(tid, editor.propId).call().foreach { replacementInfo =>
      val newEditor = new PropValueEditor(replacementInfo, this)
      // TBD: Do we also need to update the section's doRender? That would require pulling out that props.map below: 
      $(editor.elem).replaceWith(newEditor.render)
      Gadgets.hookPendingGadgets()
      PageManager.currentPage.foreach(_.reindex())
      after
    }
  }
  
  def doRender() = 
    ul(cls:="_propertySection form-horizontal",
      // Note that the name for the Instance Property section is the path of the Instance Props Property:
      name:=nam, 
      // Needed for save() to work:
      data("thing"):=tid.underlying,
      if (props.isEmpty)
        // Show *something*, so there is a drop target:
        // TODO: when stuff is dragged in and out, we should remove/add this dummy target when needed:
        li(cls:="_propListItem form-group _instanceEditor",
          raw("&nbsp;"))
      else
        props.map(new PropValueEditor(_, this))
    )
}