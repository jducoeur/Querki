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
import querki.display.{RawDiv, WithTooltip}
import querki.display.input.InputGadget
import querki.pages._

class ModelDesignerPage(params:ParamMap)(implicit e:Ecology) extends Page(e) with EcologyMember  {
  
  lazy val modelId = params("modelId")
  
  lazy val Client = interface[querki.client.Client]
  lazy val Gadgets = interface[querki.display.Gadgets]
  
  override def beforeRender() = {
    // Page-specific gadget hooks:
    // TODO: these need to be updated for the new Bootstrap. Can we come up with a better abstraction here?
    Gadgets.registerHook("._largeTextEdit") { elem => $(elem).addClass("span10") }
    Gadgets.registerHook("input[type='text']") { elem => $(elem).filter(".propEditor").addClass("span10") }    
  }
  
  def makeEditor(info:PropEditInfo):scalatags.JsDom.TypedTag[dom.HTMLLIElement] = {
    val prompt = info.prompt.map(_.raw.toString).getOrElse(info.displayName)
    // TODO: there is a nasty bug here. The tooltip should be normally wiki-processed,
    // but there is no way to use raw() on an attribute value. So we instead are displaying
    // the raw, unprocessed form, knowing that Scalatags will escape it.
    val tooltip = info.tooltip.map(_.plaintext).getOrElse(info.displayName)
    li(cls:="_propListItem control-group",
      data("propid"):=info.propId,
      new WithTooltip(label(cls:="_propPrompt control-label", raw(s"$prompt ")), tooltip),
      new RawDiv(info.editor, cls:="controls")
    )
  }
  
  def addProperty(propId:String) = {
    Client[EditFunctions].addPropertyAndGetEditor(modelId, propId).call().foreach { editInfo =>
      // TODO: introduce the concept of Properties that are mainly for Models; if that is
      // set, put it in the Model section instead:
      instancePropSection().appendEditor(editInfo)
    }
  }
  
  class PropertySection(nam:String, props:Seq[PropEditInfo]) extends InputGadget[dom.HTMLUListElement](ecology) {
    
    /**
     * The Properties currently in this section. Note that this is a var so that more props can be added.
     */
    val propIds = Var(props.map(_.propId).toSet)
    
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
      val editor = makeEditor(editInfo)
      $(elem).append(editor.render)
      propIds() += editInfo.propId
      onMoved()
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
      (instanceProps, modelProps) = fullEditInfo.propInfos.partition(propInfo => fullEditInfo.instancePropIds.contains(propInfo.propId))
      sortedInstanceProps = (Seq.empty[PropEditInfo] /: fullEditInfo.instancePropIds) { (current, propId) =>
        instanceProps.find(_.propId == propId) match {
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

import querki.api.ThingFunctions
import querki.data.{PropInfo, SpaceProps, ThingInfo}
import querki.display.{AfterLoading, Gadget, QText, WrapperDiv}
import querki.display.rx.{RxAttr, RxDiv, RxSelect}

object ButtonKind extends Enumeration {
  type ButtonKind = Value
  
  val Normal, Info, Primary = Value
}
import ButtonKind._

/**
 * A simple Gadget that encapsulates the notion of a Button that does something when clicked. Almost
 * trivial, but this saves some boilerplate.
 */
class ButtonGadget(kind:ButtonKind, mods:Modifier*)(onClick: => Unit) extends Gadget[dom.HTMLAnchorElement] {
  def doRender() = {
    val kindStr = kind match {
      case Normal => ""
      case Info => "btn-info"
      case Primary => "btn-primary"
    }
    a(cls:=s"btn $kindStr", mods)
  }
  
  override def onCreate(e:dom.HTMLAnchorElement) = {
    $(elem).click({ evt:JQueryEventObject =>
      onClick
    })
  }
}

class AddPropertyGadget(page:ModelDesignerPage, thing:ThingInfo)(implicit val ecology:Ecology) extends Gadget[dom.HTMLDivElement] with EcologyMember {
  
  val optLabel = "label".attr
  
  lazy val Client = interface[querki.client.Client]
  lazy val DataAccess = interface[querki.data.DataAccess]
  
  lazy val mainDiv = (new WrapperDiv).initialContent(initButton)
  
  lazy val initButton:ButtonGadget = new ButtonGadget(ButtonKind.Info, icon("plus"), " Add a Property")({
    mainDiv.replaceContents(addExisting.rendered, true)
  })
  
  // Note that we pro-actively begin loading this immediately. It's one of the more common operations for the
  // Model Designer, and we want quick response.
  val allPropsFut = DataAccess.getAllProps()
  val stdInfoFut = DataAccess.standardInfo
  
  class AddExistingPropertyGadget(mainSpaceProps:SpaceProps) extends Gadget[dom.HTMLDivElement] {

    // The add button is only enabled when the selection is non-empty; when pressed, it tells the parent
    // page to add the Property:
    lazy val addButton = new ButtonGadget(Info, RxAttr("disabled", Rx{ selectedProperty().isEmpty }), "Add")({
      page.addProperty(selectedProperty().get)
      mainDiv.replaceContents(initButton.rendered, true)
    })
    
    lazy val propSelector = RxSelect(propOptions)
    
    lazy val existingPropIds = Rx { page.instancePropSection().propIds() ++ page.modelPropSection().propIds() }
    
    // The currently-valid options to show in the propSelector. Note that this reactively depends on the
    // properties that already exist.
    lazy val propOptions = Rx {
      // The SpaceProps are actually a tree: each level contains this Space's Properties, and the
	  // SpaceProps for its Apps. So we do a recursive dive to build our options:
	  def processProps(spaceProps:SpaceProps):Seq[Frag] = {
	   
	    def processPropSection(prefix:String, allProps:Seq[PropInfo]):Option[Frag] = {
	      // Filter out Properties that don't apply to this Thing, or are already in use:
	      val props = 
	        allProps.
	          filter(_.appliesTo.map(_ == thing.kind).getOrElse(true)).
	          filter(prop => !existingPropIds().contains(prop.oid)).
	          sortBy(_.name)
	        
	      if (props.isEmpty)
	        None
	      else
	        Some(optgroup(optLabel:=s"$prefix Properties in ${spaceProps.spaceName}",
	          props.map { prop => option(value:=prop.oid, prop.name) }
	        ))
	    }
	    
	    FSeq(
	      processPropSection("", spaceProps.standardProps),
	      processPropSection("Advanced ", spaceProps.advancedProps),
	      spaceProps.apps.flatMap(processProps(_))
	    )
	  }
	    
	  option("Choose a Property...", value:="") +: processProps(mainSpaceProps)
    }
    
    lazy val selectedProperty = propSelector.selectedValOpt
    
    lazy val emptyDescription = span(raw("&nbsp;"))
    lazy val selectedPropertyDescriptionFut:Rx[Future[Gadget[dom.Element]]] = Rx {
      selectedProperty() match {
        case Some(propId) => {
          for {
            stdInfo <- stdInfoFut
            summaryOpt <- Client[ThingFunctions].getPropertyDisplay(propId, stdInfo.summaryPropId).call()
            detailsOpt <- Client[ThingFunctions].getPropertyDisplay(propId, stdInfo.detailsPropId).call()
          }
            yield
              // ... build the display of the Property info...
              div(
                b(propSelector.selectedText()),
                summaryOpt.map(summary => i(new QText(summary))),
                detailsOpt.map(details => new QText(details))
              )
        }
        
        case None => Future.successful(emptyDescription)
      }
    }
    lazy val selectedPropertyDescription = selectedPropertyDescriptionFut.async(emptyDescription)
    
    lazy val propertyDescriptionDiv = RxDiv(Rx {Seq(selectedPropertyDescription())} )

    
    def doRender() = {
      val result = div(cls:="well container span12",
        p(i(cls:="fa fa-spinner fa-spin"), """Choose a property from this list of existing properties, or press "Create a New Property" to do something different."""),
        div(cls:="span4",
          p(cls:="offset1",
            propSelector
          ),
          p(cls:="offset1",
            addButton,
            new ButtonGadget(Normal, "Cancel")({})
          ),
          hr,
          p(new ButtonGadget(Info, "Create a new Property instead")({}))
        ),
        div(cls:="span7", propertyDescriptionDiv)
      )
      result
    }
  }
  
  lazy val addExisting = AfterLoading(allPropsFut) { spaceProps => 
    new AddExistingPropertyGadget(spaceProps)
  }
  
  def doRender() = {
    div(mainDiv)
  }
}
