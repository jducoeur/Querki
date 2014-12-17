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
import querki.data.{PropInfo, SpaceProps, ThingInfo}
import querki.display.{ButtonGadget, ButtonKind, Gadget, RawDiv, WithTooltip}
import querki.display.input.{DeleteInstanceButton, InputGadget}
import querki.display.rx.{RxDiv, RxThingSelector}
import querki.pages._

class ModelDesignerPage(params:ParamMap)(implicit e:Ecology) extends Page(e) with EcologyMember  {
  
  lazy val modelId = TID(params("modelId"))
  
  lazy val Client = interface[querki.client.Client]
  lazy val DataModel = interface[querki.datamodel.DataModel]
  lazy val Gadgets = interface[querki.display.Gadgets]
  lazy val StatusLine = interface[querki.display.StatusLine]
  
  // We start loading this at page load, so that it's available when we inspect Properties.
  val allTypesFut = DataAccess.getAllTypes()
  // Note that we pro-actively begin loading this immediately. It's one of the more common operations for the
  // Model Designer, and we want quick response.
  val allPropsFut = DataAccess.getAllProps()
  
  /**
   * This is a Future of a Map of all of the known Properties, by OID.
   */
  val propMapFut = allPropsFut.map { mainSpaceProps =>
    def spacePropsRec(spaceProps:SpaceProps):Seq[PropInfo] = {
      spaceProps.standardProps ++ spaceProps.advancedProps ++ spaceProps.apps.flatMap(spacePropsRec(_))
    }
    
    val allProps = spacePropsRec(mainSpaceProps)
    Map(allProps.map(propInfo => (propInfo.oid -> propInfo)):_*)
  }
  
  /**
   * This is a Future of a Map of all the known Types, by OID.
   */
  val typeMapFut = allTypesFut.map { allTypeInfo =>
    val allTypes = allTypeInfo.standardTypes ++ allTypeInfo.advancedTypes
    Map(allTypes.map(typeInfo => (typeInfo.oid -> typeInfo)):_*)
  }
  
  /**
   * This is a Future of a Map of all the known Collections, by OID.
   */
  val collMapFut = allTypesFut.map { allTypeInfo =>
    Map(allTypeInfo.collections.map(collInfo => (collInfo.oid -> collInfo)):_*)
  }
  
  /**
   * This is a Future of a Map of all the usable Models, by OID.
   */
  val modelMapFut = allTypesFut.map { allTypeInfo =>
    Map(allTypeInfo.models.map(modelInfo => (modelInfo.oid -> modelInfo)):_*)
  }
  
  override def beforeRender() = {
    // Page-specific gadget hooks:
    // TODO: these need to be updated for the new Bootstrap. Can we come up with a better abstraction here?
    Gadgets.registerHook("._largeTextEdit") { elem => $(elem).addClass("span10") }
    Gadgets.registerHook("input[type='text']") { elem => $(elem).filter(".propEditor").addClass("span10") }    
  }
  
  def page = this
  
  class PropertyDetails(val valEditor:PropValueEditor) extends Gadget[dom.HTMLDivElement] {
    def editInfo = valEditor.info
    def propInfo = editInfo.propInfo
    
    // Mimic the RxSelect that the rest of the code passes in to DescriptionDiv:
    val thingSelector = 
      new RxThingSelector {
        val selectedText = Var(propInfo.displayName)
        def selectedTID = Var(propInfo.oid)
      } 
    val selectorWrapper = Var[Option[(RxThingSelector, TID)]](None)
    val descDiv = new DescriptionDiv(page, selectorWrapper)
    val propertyDescriptionDiv = descDiv.descriptionDiv
    selectorWrapper() = Some((thingSelector, propInfo.oid))
    
    def doRender() = {
      div(
        hr,
        propertyDescriptionDiv,
        if (editInfo.canEditProperty) {
          p(new ButtonGadget(ButtonKind.Primary, "Edit Property")({ valEditor.showPropEditor() }))        
        }
      )
    }
  }
  
  class PropertyEditor(val valEditor:PropValueEditor) extends Gadget[dom.HTMLDivElement] {
    lazy val propId = valEditor.propInfo.oid
    lazy val empty = ul()
    lazy val guts = Var[Gadget[dom.HTMLUListElement]](empty)
    lazy val contentDiv = RxDiv(Rx {Seq(guts())})
    lazy val contentFut = {
      for {
        editInfo <- Client[EditFunctions].getPropertyEditors(propId).call()
      }
        yield new PropertySection(s"Property $propId", editInfo.propInfos, propId, false)
    }
    lazy val editTrigger = contentFut.foreach { section => 
      guts() = section
      hookWhenDone
    }
    lazy val hookWhenDone = Obs(contentDiv.elemRx) {
      InputGadgets.hookPendingGadgets()      
    }
    
    def doRender() = {
      editTrigger
      div(
        hr,
        contentDiv,
        p(new ButtonGadget(ButtonKind.Primary, "Done")({ 
          valEditor.propEditDone() 
        }))
      )
    }
  }
  
  class PropValueEditor(val info:PropEditInfo, val section:PropertySection, openEditorInitially:Boolean = false) extends Gadget[dom.HTMLLIElement] {
    val propInfo = info.propInfo
    val propId = propInfo.oid
    val prompt = info.prompt.map(_.raw.toString).getOrElse(propInfo.displayName)
    // TODO: there is a nasty bug here. The tooltip should be normally wiki-processed,
    // but there is no way to use raw() on an attribute value. So we instead are displaying
    // the raw, unprocessed form, knowing that Scalatags will escape it.
    val tooltip = info.tooltip.map(_.plaintext).getOrElse(propInfo.displayName)
    
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
          new DeleteInstanceButton({() => removeProperty(this)}), 
          raw(s"$prompt ")),
          tooltip),
        new RawDiv(info.editor, cls:="controls"),
        propDetailsArea
      )
      
    override def onCreate(e:dom.HTMLLIElement) = {
      if (openEditorInitially) 
        showPropEditor()
    }
  }
  
  def addProperty(propId:TID, openEditor:Boolean = false) = {
    Client[EditFunctions].addPropertyAndGetEditor(modelId, propId).call().foreach { editInfo =>
      // TODO: introduce the concept of Properties that are mainly for Models; if that is
      // set, put it in the Model section instead:
      instancePropSection().appendEditor(editInfo, openEditor)
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
  
  class PropertySection(nam:String, props:Seq[PropEditInfo], thingId:TID, sortable:Boolean = true) extends InputGadget[dom.HTMLUListElement](ecology) {
    
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
      if (sortable) {
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
    }
    
    def appendEditor(editInfo:PropEditInfo, openEditor:Boolean) = {
      val editor = new PropValueEditor(editInfo, this, openEditor)
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
    
    def refreshEditor(editor:PropValueEditor) = {
      Client[EditFunctions].getOnePropertyEditor(thingId, editor.propId).call().foreach { replacementInfo =>
        val newEditor = new PropValueEditor(replacementInfo, this)
        // TBD: Do we also need to update the section's doRender? That would require pulling out that props.map below: 
        $(editor.elem).replaceWith(newEditor.render)
        InputGadgets.hookPendingGadgets()
      }
    }
    
    def doRender() = 
      ul(cls:="_propertySection form-horizontal",
        // Note that the name for the Instance Property section is the path of the Instance Props Property:
        name:=nam, 
        // Needed for save() to work:
        data("thing"):=thingId.underlying,
        props.map(new PropValueEditor(_, this))
      )
  }
  
  class PropSectionHolder {
    var _propSection:Option[PropertySection] = None
    def make(sortedProps:Seq[PropEditInfo], path:String) = {
      _propSection = Some(new PropertySection(path, sortedProps, modelId))
      _propSection
    }
    def apply() = _propSection.get    
  }
  val instancePropSection = new PropSectionHolder
  val modelPropSection = new PropSectionHolder

  def pageContent = {
    for {
      model <- DataAccess.getThing(modelId)
      modelModel <- DataAccess.getThing(model.modelOid)
      fullEditInfo <- Client[EditFunctions].getPropertyEditors(modelId).call()
      (instanceProps, modelProps) = fullEditInfo.propInfos.partition(propEditInfo => fullEditInfo.instancePropIds.contains(propEditInfo.propInfo.oid))
      sortedInstanceProps = (Seq.empty[PropEditInfo] /: fullEditInfo.instancePropIds) { (current, propId) =>
        instanceProps.find(_.propInfo.oid == propId) match {
          case Some(prop) => current :+ prop
          case None => { println(s"Couldn't find property $propId, although it is in instancePropIds!"); current }
        }
      }
      pageTitle = {
	    if (model.isModel)
	      s"Designing Model ${model.displayName}"
	    else
	      s"Editing ${model.displayName}"
      }
	  guts = 
        div(cls:="_advancedEditor",
          h1(pageTitle),
          p(cls:="_smallSubtitle", 
            s"Model: ${modelModel.displayName} -- ",
            a("Change Model", 
              href:=PageManager.currentHash,
              onclick:={ () => 
                DataModel.changeModel(
                  model,
                  { newThingInfo => PageManager.reload() }) 
            })
          ),
          h3(cls:="_defaultTitle", "Instance Properties"),
          p(cls:="_smallSubtitle", 
              """These are the Properties that can be different for each Instance. Drag a Property into here if you
                |want to edit it for each Instance, or out if you don't. The order of the Properties here will be
                |the order they show up in the Instance Editor.""".stripMargin),
          instancePropSection.make(sortedInstanceProps, fullEditInfo.instancePropPath),
          new AddPropertyGadget(this, model),
          h3(cls:="_defaultTitle", "Model Properties"),
          p(cls:="_smallSubtitle", "These Properties are the same for all Instances of this Model"),
          modelPropSection.make(modelProps, "modelProps"),
          a(cls:="btn btn-primary",
            "Done",
            href:=thingUrl(model))
        )
    }
      yield PageContents(pageTitle, guts)
  }
}
