package querki.editing

import scala.scalajs.js
import org.scalajs.dom
import org.scalajs.jquery._
import org.scalajs.jqueryui._
import scalatags.JsDom.all._
import autowire._
import rx._
import rx.ops._

import querki.globals._

import querki.api.EditFunctions
import EditFunctions._
import querki.data.ThingInfo
import querki.display.{Gadget, RawDiv, WithTooltip}
import querki.display.input.{DeleteInstanceButton, InputGadget}
import querki.display.rx.{RxDiv, RxThingSelector}
import querki.pages._

class ModelDesignerPage(params:ParamMap)(implicit e:Ecology) extends Page(e) with EcologyMember  {
  
  lazy val modelId = params("modelId")
  
  lazy val Client = interface[querki.client.Client]
  lazy val Gadgets = interface[querki.display.Gadgets]
  lazy val StatusLine = interface[querki.display.StatusLine]
  
  override def beforeRender() = {
    // Page-specific gadget hooks:
    // TODO: these need to be updated for the new Bootstrap. Can we come up with a better abstraction here?
    Gadgets.registerHook("._largeTextEdit") { elem => $(elem).addClass("span10") }
    Gadgets.registerHook("input[type='text']") { elem => $(elem).filter(".propEditor").addClass("span10") }    
  }
  
  class PropertyDetails(val valEditor:PropValueEditor) extends Gadget[dom.HTMLDivElement] {
    def editInfo = valEditor.info
    def propInfo = editInfo.propInfo
    val thingSelector = 
      new RxThingSelector {
        val selectedText = Var(propInfo.displayName)
        def selectedVal = Var(propInfo.oid)
      } 
    
    def doRender() =
      div(
        hr
//        p("TODO: collection and type"),
//        p("TODO: editors for Summary and Details"),
//        p("TODO: editors for standard fields for this Type")
      )
  }
  
  class PropValueEditor(val info:PropEditInfo, val section:PropertySection) extends Gadget[dom.HTMLLIElement] {
    val propInfo = info.propInfo
    val prompt = info.prompt.map(_.raw.toString).getOrElse(propInfo.displayName)
    // TODO: there is a nasty bug here. The tooltip should be normally wiki-processed,
    // but there is no way to use raw() on an attribute value. So we instead are displaying
    // the raw, unprocessed form, knowing that Scalatags will escape it.
    val tooltip = info.tooltip.map(_.plaintext).getOrElse(propInfo.displayName)
    
    // Functions to toggle the PropertyEditor in and out when you click the name of the property:
    val detailsShown = Var(false)
    val detailsHolder = Var[Seq[Gadget[_]]](Seq.empty)
    lazy val detailsEditor = new PropertyDetails(this)
    val propDetailsArea = new RxDiv(detailsHolder, display:="none", width:="100%")
    def toggleDetails() = {
      detailsHolder() = Seq(detailsEditor)
      if (detailsShown()) {
        propDetailsArea.elemOpt.map($(_).slideUp())
      } else {
        propDetailsArea.elemOpt.map($(_).slideDown())
      }
      detailsShown() = !detailsShown()
    }
    
    def doRender() = 
      // HACK: we're calling this _instanceEditor in order to make the DeleteButton's style work. Let's
      // refactor this somehow:
      li(cls:="_propListItem control-group _instanceEditor",
        data("propid"):=propInfo.oid,
        new WithTooltip(label(cls:="_propPrompt control-label", 
          onclick:={ () => toggleDetails() },
          new DeleteInstanceButton({() => removeProperty(this)}), 
          raw(s"$prompt ")),
          tooltip),
        new RawDiv(info.editor, cls:="controls"),
        propDetailsArea
      )
  }
  
  def addProperty(propId:String) = {
    Client[EditFunctions].addPropertyAndGetEditor(modelId, propId).call().foreach { editInfo =>
      // TODO: introduce the concept of Properties that are mainly for Models; if that is
      // set, put it in the Model section instead:
      instancePropSection().appendEditor(editInfo)
      InputGadgets.hookPendingGadgets()
    }
  }
  
  def removeProperty(editor:PropValueEditor) = {
    Client[EditFunctions].removeProperty(modelId, editor.propInfo.oid).call().foreach { result =>
      result match {
        case PropertyChanged => editor.section.removeEditor(editor)
        case PropertyChangeError(msg) => StatusLine.showBriefly(msg)        
      }
    }
  }
  
  class PropertySection(nam:String, props:Seq[PropEditInfo]) extends InputGadget[dom.HTMLUListElement](ecology) {
    
    /**
     * The Properties currently in this section. Note that this is a var so that more props can be added.
     */
    val propIds = Var(props.map(_.propInfo.oid).toSet)
    
    // Note that this is only ever invoked on the Instance Property Section:
    def values = {
      $(elem).children("li").map({ propElem:dom.Element =>
        $(propElem).data("propid")
      }:js.ThisFunction0[dom.Element, Any]).jqf.get().asInstanceOf[js.Array[String]]
    }
  
    // Note that this is only ever invoked on the Instance Property Section:
    def onMoved() = {
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
          instancePropSection().onMoved()
        }:js.Function2[JQueryEventObject, SortChangeUI, Any]
      ))
    }
    
    def appendEditor(editInfo:PropEditInfo) = {
      val editor = new PropValueEditor(editInfo, this)
      $(elem).append(editor.rendered)
      propIds() += editInfo.propInfo.oid
      onMoved()
    }
    
    def removeEditor(editor:PropValueEditor) = {
      val child = $(editor.elem)
      child.hide(400, { () => child.remove() })
      propIds() -= editor.info.propInfo.oid
      instancePropSection().onMoved()
    }
    
    def doRender() = 
      ul(cls:="_propertySection form-horizontal",
        // Note that the name for the Instance Property section is the path of the Instance Props Property:
        name:=nam, 
        // Needed for save() to work:
        data("thing"):=modelId,
        props.map(new PropValueEditor(_, this))
      )
  }
  
  class PropSectionHolder {
    var _propSection:Option[PropertySection] = None
    def make(sortedProps:Seq[PropEditInfo], path:String) = {
      _propSection = Some(new PropertySection(path, sortedProps))
      _propSection
    }
    def apply() = _propSection.get    
  }
  val instancePropSection = new PropSectionHolder
  val modelPropSection = new PropSectionHolder

  def pageContent = {
    for {
      standardInfo <- DataAccess.standardInfo
      model <- DataAccess.getThing(modelId)
      fullEditInfo <- Client[EditFunctions].getPropertyEditors(modelId).call()
      (instanceProps, modelProps) = fullEditInfo.propInfos.partition(propEditInfo => fullEditInfo.instancePropIds.contains(propEditInfo.propInfo.oid))
      sortedInstanceProps = (Seq.empty[PropEditInfo] /: fullEditInfo.instancePropIds) { (current, propId) =>
        instanceProps.find(_.propInfo.oid == propId) match {
          case Some(prop) => current :+ prop
          case None => { println(s"Couldn't find property $propId, although it is in instancePropIds!"); current }
        }
      }
      guts = 
        div(cls:="_advancedEditor",
          h3(cls:="_defaultTitle", "Instance Properties"),
          p(cls:="_smallSubtitle", 
              """These are the Properties that can be different for each Instance. Drag a Property into here if you
                |want to edit it for each Instance, or out if you don't. The order of the Properties here will be
                |the order they show up in the Instance Editor.""".stripMargin),
          instancePropSection.make(sortedInstanceProps, fullEditInfo.instancePropPath),
          new AddPropertyGadget(this, model),
          h3(cls:="_defaultTitle", "Model Properties"),
          p(cls:="_smallSubtitle", "These Properties are the same for all Instances of this Model"),
          modelPropSection.make(modelProps, "modelProps")
        )
    }
      yield PageContents(s"Designing Model ${model.displayName}", guts)
  }
}
