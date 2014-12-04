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
          h3(cls:="_defaultTitle", "Instance Properties"),
          p(cls:="_smallSubtitle", 
              """These are the Properties that can be different for each Instance. Drag a Property into here if you
                |want to edit it for each Instance, or out if you don't. The order of the Properties here will be
                |the order they show up in the Instance Editor.""".stripMargin),
          makeInstancePropSection(sortedInstanceProps, fullEditInfo.instancePropPath),
          new AddPropertyGadget,
          h3(cls:="_defaultTitle", "Model Properties"),
          p(cls:="_smallSubtitle", "These Properties are the same for all Instances of this Model"),
          new PropertySection("modelProps", modelProps)
        )
    }
      yield PageContents(s"Designing Model ${model.displayName}", guts)
  }
  
}

import querki.display.{Gadget, MetaGadget, WrapperDiv}

class AfterLoading[T, Output <: dom.Element](fut:Future[T])(guts:T => scalatags.JsDom.TypedTag[Output]) extends MetaGadget[dom.HTMLDivElement] {
  // TODO: once we upgrade to Bootstrap 3, we should switch to FontAwesome and use the spinners in that:
  lazy val wrapper = (new WrapperDiv).initialContent("Loading...")
  
  def doRender() = wrapper
  
  fut.map { result =>
    val finalTag = guts(result)
    wrapper.replaceContents(finalTag.render)
  }
}
object AfterLoading {
  def apply[T, Output <: dom.Element](fut:Future[T])(guts:T => scalatags.JsDom.TypedTag[Output]) = new AfterLoading(fut)(guts)
}

/**
 * A simple Gadget that encapsulates the notion of a Button that does something when clicked. Almost
 * trivial, but this saves some boilerplate.
 */
class ButtonGadget(mods:Modifier*)(onClick: => Unit) extends Gadget[dom.HTMLAnchorElement] {
  def doRender() = a(cls:="btn btn-info", mods)
  
  override def onCreate(e:dom.HTMLAnchorElement) = {
    $(elem).click({ evt:JQueryEventObject =>
      onClick
    })
  }
}

/*
          <div id="_addExistingPropertyBox">
            <p>Choose a property from this list of existing properties, or press "Create a New Property" to do something different.</p>
            <div class="span4">
              <p class="offset1">
                <select id="_propChooser" data-placeholder="Choose a Property...">
                  <option value="">Choose a Property...</option>
                  @propsInSpace(space)
                </select>
              </p>
              <p class="offset1">
                <input type="button" value="Add" id="_addExistingPropertyButton" class="btn btn-info">
                <input type="button" value="Cancel" id="_cancelAddPropertyButton" class="btn">
              </p>
              <hr/>
              <p><input type="button" value="Create a New Property instead" id="_createPropertyButton" class="btn btn-info"></p>
            </div>
            <div class="span7">
              <p id="_propInfo">&nbsp;</p>
            </div>
          </div>
 */

class AddPropertyGadget(implicit val ecology:Ecology) extends Gadget[dom.HTMLDivElement] with EcologyMember {
  
  lazy val DataAccess = interface[querki.data.DataAccess]
  
  lazy val mainDiv = (new WrapperDiv).initialContent(initButton)
  
  lazy val initButton:ButtonGadget = new ButtonGadget(icon("plus"), " Add a Property")({
    mainDiv.replaceContents(addExisting.render)
  })
  
  // Note that we pro-actively begin loading this immediately. It's one of the more common operations for the
  // Model Designer, and we want quick response.
  val allPropsFut = DataAccess.getAllProps()
  
  class AddExistingPropertyGadget extends Gadget[dom.HTMLDivElement] {
    def doRender() =
      div(cls:="well container",
        p(i(cls:="fa fa-spinner fa-spin"), """Choose a property from this list of existing properties, or press "Create a New Property" to do something different."""),
        div(cls:="span4",
          p(cls:="offset1",
            AfterLoading(allPropsFut) { spaceProps =>
              p(s"Props for ${spaceProps.spaceName} will go here")
            }
          )
        )
      )
  }
  
  lazy val addExisting = new AddExistingPropertyGadget
  
  def doRender() = {
    div(mainDiv)
  }
}
