package querki.editing

import org.scalajs.dom
import org.querki.jquery._
import scalatags.JsDom.all._
import rx._

import querki.globals._

import querki.api.EditFunctions
import EditFunctions.PropEditInfo
import querki.display.{Gadget, RawDiv, WithTooltip}
import querki.display.input.DeleteInstanceButton
import querki.display.rx.RxDiv
  
class PropValueEditor(val info:PropEditInfo, val section:PropertySection, openEditorInitially:Boolean = false)(implicit val ecology:Ecology) 
  extends Gadget[dom.HTMLLIElement] 
{
    val propInfo = info.propInfo
    val propId = propInfo.oid
    val prompt = info.prompt.map(_.raw.toString).getOrElse(propInfo.displayName)
    // TODO: there is a nasty bug here. The tooltip should be normally wiki-processed,
    // but there is no way to use raw() on an attribute value. So we instead are displaying
    // the raw, unprocessed form, knowing that Scalatags will escape it.
    val tooltip = info.tooltip.map(_.plaintext).getOrElse(propInfo.displayName)
    def stdThings = section.stdThings
    
    // Functions to toggle the PropertyEditor in and out when you click the name of the property:
    val detailsShown = Var(false)
    val detailsHolder = Var[Seq[Gadget[dom.HTMLDivElement]]](Seq.empty, name="detailsHolder")
    lazy val detailsViewer = new PropertyDetails(this)
    lazy val detailsViewerSeq = Seq(detailsViewer)
    val propDetailsArea = new RxDiv(detailsHolder, display:="none", width:="100%")
    def toggleDetails() = {
      detailsHolder() = detailsViewerSeq
      if (detailsShown()) {
        propDetailsArea.elemOpt.map($(_).slideUp())
      } else {
        propDetailsArea.elemOpt.map($(_).slideDown())
      }
      detailsShown() = !detailsShown()
    }
    
    lazy val propEditor = new PropertyEditor(this)
    lazy val propEditorSeq = Seq(propEditor)
    def showPropEditor() = {
      detailsHolder() = propEditorSeq
      propDetailsArea.elemOpt.map($(_).show())
      detailsShown() = true
    }
    
    def propEditDone() = {
      detailsHolder() = Seq.empty
      toggleDetails()
      section.refreshEditor(this)
    }
    
    def doRender() = 
      // HACK: we're calling this _instanceEditor in order to make the DeleteButton's style work. Let's
      // refactor this somehow:
      li(cls:="_propListItem control-group _instanceEditor",
        data("propid"):=propInfo,
        new WithTooltip(label(cls:="_propPrompt control-label", 
          onclick:={ () => toggleDetails() },
          new DeleteInstanceButton({() => section.page.removeProperty(this)}), 
          raw(s"$prompt ")),
          tooltip),
        new RawDiv(info.editor, cls:="controls"),
        if (propId == stdThings.basic.displayNameProp.oid)
          new DeriveNameCheck(this),
        propDetailsArea
      )
      
    override def onCreate(e:dom.HTMLLIElement) = {
      if (openEditorInitially) 
        showPropEditor()
    }
  }
