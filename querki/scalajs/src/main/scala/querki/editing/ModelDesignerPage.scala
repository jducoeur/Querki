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
  
  def makeEditor(info:PropEditInfo):Modifier = {
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
    println(s"Time to add property $propId")
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
          makeInstancePropSection(sortedInstanceProps, fullEditInfo.instancePropPath),
          new AddPropertyGadget(this, model),
          h3(cls:="_defaultTitle", "Model Properties"),
          p(cls:="_smallSubtitle", "These Properties are the same for all Instances of this Model"),
          new PropertySection("modelProps", modelProps)
        )
    }
      yield PageContents(s"Designing Model ${model.displayName}", guts)
  }
  
}

import rx._
import querki.api.ThingFunctions
import querki.data.{PropInfo, SpaceProps, ThingInfo}
import querki.display.{AfterLoading, Gadget, QText, RxAttr, WrapperDiv}

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
    mainDiv.replaceContents(addExisting.render)
  })
  
  lazy val propDesc = (new WrapperDiv).initialContent(raw("&nbsp;"))
  
  // Note that we pro-actively begin loading this immediately. It's one of the more common operations for the
  // Model Designer, and we want quick response.
  val allPropsFut = DataAccess.getAllProps()
  val stdInfoFut = DataAccess.standardInfo
  
  class AddExistingPropertyGadget extends Gadget[dom.HTMLDivElement] {
    
    val selectedProperty = Var[Option[String]](None)

    // The add button is only enabled when the selection is non-empty; when pressed, it tells the parent
    // page to add the Property:
    lazy val addButton = new ButtonGadget(Info, RxAttr("disabled", Rx{ selectedProperty().isEmpty }), "Add")({
      page.addProperty(selectedProperty().get)
    })
    
    // The SpaceProps are actually a tree: each level contains this Space's Properties, and the
    // SpaceProps for its Apps. So we do a recursive dive to build our options:
    def processProps(spaceProps:SpaceProps):Seq[Modifier] = {
      
      def processPropSection(prefix:String, allProps:Seq[PropInfo]):Option[Modifier] = {
        // Filter out Properties that don't apply to this Thing:
        val props = allProps.filter(_.appliesTo.map(_ == thing.kind).getOrElse(true)).sortBy(_.name)
        
        if (props.isEmpty)
          None
        else
          Some(optgroup(optLabel:=s"$prefix Properties in ${spaceProps.spaceName}",
            props.map { prop => option(value:=prop.oid, prop.name) }
          ))
      }
    
      MSeq(
        processPropSection("", spaceProps.standardProps),
        processPropSection("Advanced ", spaceProps.advancedProps),
        spaceProps.apps.flatMap(processProps(_))
      )
    }
    
    def doRender() =
      div(cls:="well container span12",
        p(i(cls:="fa fa-spinner fa-spin"), """Choose a property from this list of existing properties, or press "Create a New Property" to do something different."""),
        div(cls:="span4",
          p(cls:="offset1",
            AfterLoading(allPropsFut) { spaceProps =>
              Gadget(
                // The actual select of which Property you want
                select(
                  processProps(spaceProps)
                ),
                // When the user selects a Property...
                { e =>
                  $(e).change({ evt:JQueryEventObject =>
                    val selected = $(e).find("option:selected")
                    val propId = selected.value().asInstanceOf[String]
                    // ... set the Selection (which is a reactive, and things are listening for it)...
                    selectedProperty() = Some(propId)
                    // ... fetch the Summary and Details for that Property...
                    val contentsFut = for {
                      stdInfo <- stdInfoFut
                      summaryOpt <- Client[ThingFunctions].getPropertyDisplay(propId, stdInfo.summaryPropId).call()
                      detailsOpt <- Client[ThingFunctions].getPropertyDisplay(propId, stdInfo.detailsPropId).call()
                    }
                      yield
                        // ... build the display of the Property info...
                        div(
                          b(selected.text()),
                          summaryOpt.map(summary => i(new QText(summary))),
                          detailsOpt.map(details => new QText(details))
                        )
                      
                    // ... and stuff it into the div that's waiting for it.
                    contentsFut.map { contents => 
                      propDesc.replaceContents(contents.render)
                    }
                  })
                }
              )
            }
          ),
          p(cls:="offset1",
            addButton,
            new ButtonGadget(Normal, "Cancel")({})
          ),
          hr,
          p(new ButtonGadget(Info, "Create a new Property instead")({}))
        ),
        div(cls:="span7", propDesc)
      )
  }
  
  lazy val addExisting = new AddExistingPropertyGadget
  
  def doRender() = {
    div(mainDiv)
  }
}
