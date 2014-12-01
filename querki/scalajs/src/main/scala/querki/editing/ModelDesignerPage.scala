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
import querki.display.RawSpan
import querki.display.input.InputGadget
import querki.pages._

class ModelDesignerPage(params:ParamMap)(implicit e:Ecology) extends Page(e) with EcologyMember  {
  
  lazy val modelId = params("modelId")
  
  lazy val Client = interface[querki.client.Client]
  
  var instancePropIds = Seq.empty[String]
  
  def makeEditor(instance:Boolean, info:PropEditInfo):Modifier = {
    val prompt = info.prompt.map(_.raw.toString).getOrElse(info.displayName)
    // TODO: there is a nasty bug here. The tooltip should be normally wiki-processed,
    // but there is no way to use raw() on an attribute value. So we instead are displaying
    // the raw, unprocessed form, knowing that Scalatags will escape it.
    val tooltip = info.tooltip.map(_.plaintext).getOrElse(info.displayName)
    li(cls:="_withTooltip", title:=tooltip,
      data("propid"):=info.propId,
      raw(s"$prompt (${info.propId}): "),
      new RawSpan(info.editor)
    )
  }
  
  class PropertySection(nam:String, instances:Boolean, props:Seq[PropEditInfo]) extends InputGadget[dom.HTMLUListElement](ecology) {
    def values = {
      $(elem).children("li").map({ propElem:dom.Element =>
        $(propElem).data("propid")
      }:js.ThisFunction0[dom.Element, Any]).jqf.get().asInstanceOf[js.Array[String]]
    }
  
    def onMoved(item:JQuery) = {
      save()
//      println(s"Instance props are now ${values.mkString(", ")}, and path is ${$(elem).attr("name")}")
    }
    
    def hook() = {
      $(elem).find("._propertySection").each({ prop:dom.Element =>
      }:js.ThisFunction0[dom.Element, Any])

      $(elem).sortable(SortableOptions.
        connectWith("._propertySection").
        // Stop gets called after a drag-and-drop event:
        stop({ (evt:JQueryEventObject, ui:SortChangeUI) =>
          val item = ui.item.get
          val nowIn = item.parent
          println(s"Moved from $instances to ${nowIn.data("instances")}")
          instancePropSection.onMoved(item)
//        val oldIndex = item.data("index").asInstanceOf[Int]
//        val newIndex = sortList.children("li").index(item)
//        saveChange({ path => MoveListItem(path, oldIndex, newIndex) })
//        numberItems()
        }:js.Function2[JQueryEventObject, SortChangeUI, Any]
      ))
    }
    
    def doRender() = 
      ul(cls:="_propertySection", name:=nam, 
        data("instances"):=instances,
        // Needed for save() to work:
        data("thing"):=modelId,
        props.map(makeEditor(instances, _))
      )
  }
  
  var _instancePropSection:Option[PropertySection] = None
  def makeInstancePropSection(sortedInstanceProps:Seq[PropEditInfo], path:String) = {
    _instancePropSection = Some(new PropertySection(path, true, sortedInstanceProps))
    _instancePropSection
  }
  def instancePropSection = _instancePropSection.get

  def pageContent = {
    for {
      standardInfo <- DataAccess.standardInfo
      model <- DataAccess.getThing(modelId)
      fullEditInfo <- Client[EditFunctions].getThingEditors(modelId).call()
      (instanceProps, modelProps) = fullEditInfo.propInfos.partition(propInfo => fullEditInfo.instancePropIds.contains(propInfo.propId))
      instancePropIds = fullEditInfo.instancePropIds
      sortedInstanceProps = (Seq.empty[PropEditInfo] /: instancePropIds) { (current, propId) =>
        instanceProps.find(_.propId == propId) match {
          case Some(prop) => current :+ prop
          case None => { println(s"Couldn't find property $propId, although it is in instancePropIds!"); current }
        }
      }
      guts = 
        div(
          p("Drag and drop Properties by their name to rearrange them."),
          h3("Instance Properties"),
          p("These are the Properties that can be different for each Instance"),
          makeInstancePropSection(sortedInstanceProps, fullEditInfo.instancePropPath),
          querkiButton("Add a Property"),
          h3("Model Properties"),
          p("These are the Properties that are the same for all Instances of this Model"),
          new PropertySection("modelProps", false, modelProps)
        )
    }
      yield PageContents(s"Designing Model ${model.displayName}", guts)
  }
  
}