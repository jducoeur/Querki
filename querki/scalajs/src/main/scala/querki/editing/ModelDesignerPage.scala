package querki.editing

import scala.scalajs.js
import org.scalajs.dom
import org.scalajs.jquery._
import org.scalajs.jqueryui._
import scalatags.JsDom.all._
import autowire._

import querki.globals._

import querki.api.EditFunctions
import EditFunctions._
import querki.data.ThingInfo
import querki.display.{RawDiv, TooltipGadget}
import querki.display.input.InputGadget
import querki.pages._

class ModelDesignerPage(params:ParamMap)(implicit e:Ecology) extends Page(e) with EcologyMember  {
  
  lazy val modelId = params("modelId")
  
  lazy val Client = interface[querki.client.Client]
  
  def makeEditor(info:PropEditInfo):Modifier = {
    val prompt = info.prompt.map(_.raw.toString).getOrElse(info.displayName)
    // TODO: there is a nasty bug here. The tooltip should be normally wiki-processed,
    // but there is no way to use raw() on an attribute value. So we instead are displaying
    // the raw, unprocessed form, knowing that Scalatags will escape it.
    val tooltip = info.tooltip.map(_.plaintext).getOrElse(info.displayName)
    li(cls:="_propListItem control-group",
      data("propid"):=info.propId,
      new TooltipGadget(label(cls:="_propPrompt control-label", title:=tooltip, raw(s"$prompt (${info.propId}): "))),
      new RawDiv(info.editor, cls:="controls")
    )
  }
  
  class PropertySection(nam:String, props:Seq[PropEditInfo]) extends InputGadget[dom.HTMLUListElement](ecology) {
    // Note that this is only ever invoked on the Instance Property Section:
    def values = {
      $(elem).children("li").map({ propElem:dom.Element =>
        $(propElem).data("propid")
      }:js.ThisFunction0[dom.Element, Any]).jqf.get().asInstanceOf[js.Array[String]]
    }
  
    // Note that this is only ever invoked on the Instance Property Section:
    def onMoved(item:JQuery) = {
      save()
    }
    
    def hook() = {
      $(elem).sortable(SortableOptions.
        // That is, the two PropertySections are linked, and you can drag between them:
        connectWith("._propertySection").
        // Stop gets called after a drag-and-drop event:
        stop({ (evt:JQueryEventObject, ui:SortChangeUI) =>
          val item = ui.item.get
          // IMPORTANT: note that we save the Instance Props whenever there is a drop, but this
          // stop event may be coming from Model Props if the user has dragged across the boundary.
          instancePropSection.onMoved(item)
        }:js.Function2[JQueryEventObject, SortChangeUI, Any]
      ))
    }
    
    def doRender() = 
      ul(cls:="_propertySection form-horizontal",
        // Note that the name for the Instance Property section is the path of the Instance Props Property:
        name:=nam, 
        // Needed for save() to work:
        data("thing"):=modelId,
        props.map(makeEditor(_))
      )
  }
  
  var _instancePropSection:Option[PropertySection] = None
  def makeInstancePropSection(sortedInstanceProps:Seq[PropEditInfo], path:String) = {
    _instancePropSection = Some(new PropertySection(path, sortedInstanceProps))
    _instancePropSection
  }
  def instancePropSection = _instancePropSection.get

  def pageContent = {
    for {
      standardInfo <- DataAccess.standardInfo
      model <- DataAccess.getThing(modelId)
      fullEditInfo <- Client[EditFunctions].getThingEditors(modelId).call()
      (instanceProps, modelProps) = fullEditInfo.propInfos.partition(propInfo => fullEditInfo.instancePropIds.contains(propInfo.propId))
      sortedInstanceProps = (Seq.empty[PropEditInfo] /: fullEditInfo.instancePropIds) { (current, propId) =>
        instanceProps.find(_.propId == propId) match {
          case Some(prop) => current :+ prop
          case None => { println(s"Couldn't find property $propId, although it is in instancePropIds!"); current }
        }
      }
      guts = 
        div(cls:="_advancedEditor",
          p(cls:="_smallSubtitle", "Drag and drop Properties by their name to rearrange them."),
          h3(cls:="_defaultTitle", "Instance Properties"),
          p(cls:="_smallSubtitle", "These are the Properties that can be different for each Instance"),
          makeInstancePropSection(sortedInstanceProps, fullEditInfo.instancePropPath),
          querkiButton("Add a Property"),
          h3(cls:="_defaultTitle", "Model Properties"),
          p(cls:="_smallSubtitle", "These are the Properties that are the same for all Instances of this Model"),
          new PropertySection("modelProps", modelProps)
        )
    }
      yield PageContents(s"Designing Model ${model.displayName}", guts)
  }
  
}
