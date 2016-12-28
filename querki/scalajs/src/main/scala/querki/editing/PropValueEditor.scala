package querki.editing

import org.scalajs.dom.{raw => dom}
import org.querki.jquery._
import scalatags.JsDom.all._
import rx._

import org.querki.squery.FormEvents._

import querki.globals._
import querki.data._
import querki.display.{QuerkiUIUtils, RawDiv, WithTooltip}
import querki.display.input.DeleteInstanceButton
import querki.display.rx._
import EditFunctions.PropEditInfo
  
/**
 * TODO: this was originally a horizontal form, with the labels on the left and the controls on the right.
 * Something broke in the transition to Bootstrap 3. We should probably investigate getting that working again.
 */
class PropValueEditor(val info:PropEditInfo, val section:PropertySection, openEditorInitially:Boolean = false)(implicit val ecology:Ecology) 
  extends Gadget[dom.HTMLLIElement] with QuerkiUIUtils
{
    val propInfo = info.propInfo
    val propId = propInfo.oid
    val prompt = info.prompt.map(_.raw.toString).getOrElse(propInfo.displayName)
    // TODO: there is a nasty bug here. The tooltip should be normally wiki-processed,
    // but there is no way to use raw() on an attribute value. So we instead are displaying
    // the raw, unprocessed form, knowing that Scalatags will escape it.
    val tooltip = info.tooltip.map(_.plaintext).getOrElse(propInfo.displayName)
    def stdThings = section.page.std
    
    def isSpace = section.thing match {
      case t:ThingInfo => (t.kind == models.Kind.Space)
      case s:SpaceInfo => true
      case _ => false
    }
    
    val propDetailsArea = GadgetRef.of[dom.HTMLDivElement]
    // Functions to toggle the PropertyEditor in and out when you click the name of the property:
    val detailsShown = Var(false)
    lazy val detailsViewer = new PropertyDetails(this)
    def hideDetails(cb:Function[Unit, Unit]) = {
      propDetailsArea.elemOpt.map($(_).slideUp(400, cb))
      detailsShown() = false
    }
    def toggleDetails() = {
      if (detailsShown()) {
        hideDetails({ _ => })
      } else {
        propDetailsArea <~ detailsViewer
        // TODO: we'd really like to slide this down smoothly *after* the details are fetched and rendered,
        // but the pathways for that are messy, especially because Rx has no good mechanism for expressing
        // a one-shot edge-trigger. That could use an enhancement.
        propDetailsArea.elemOpt.map($(_).slideDown())
        detailsShown() = true
      }
    }
    
    lazy val propEditor = new PropertyEditor(this)
    // This gets called by PropertyDetails:
    def showPropEditor() = {
      propDetailsArea <~ propEditor
      propDetailsArea.elemOpt.map($(_).show())
      detailsShown() = true
    }
    
    // This gets called by PropertyEditor:
    def propEditDone() = {
      // Do the real changes after we slideUp:
      hideDetails({ _ => 
        section.refreshEditor(this) { newEditor =>
          // And once it's refreshed, focus to it. Note that that's a new
          // instance; this one is being thrown away afterwards:
          newEditor.focus()
        }
      })
    }
    
    def doRender() = 
      // HACK: we're calling this _instanceEditor in order to make the DeleteButton's style work. Let's
      // refactor this somehow:
      li(cls:="_propListItem _instanceEditor",
        data("propid"):=propInfo,
        div(cls:="row _propValueGuts",
          new WithTooltip(label(cls:="_propPrompt col-md-2", 
            onclick:={ () => toggleDetails() },
            raw(s"$prompt ")),
            tooltip),
          new RawDiv(info.editor, cls:="col-md-9 col-xs-11"),
          if (section.sortable)
            i(cls:="_dragHandle col-xs-1 glyphicon glyphicon-move"),
          new DeleteInstanceButton({() => section.page.removeProperty(this)}) 
        ),
        if ((propId == stdThings.basic.displayNameProp.oid) && !isSpace)
          new DeriveNameCheck(this),
        propDetailsArea <= div(display:="none", width:="100%")
      )
      
    override def onCreate(e:dom.HTMLLIElement) = {
      if (openEditorInitially) 
        showPropEditor()
    }
  }
