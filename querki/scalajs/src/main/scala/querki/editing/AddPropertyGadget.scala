package querki.editing

import org.scalajs.dom

import autowire._
import rx._
import rx.ops._
import scalatags.JsDom.all._

import querki.globals._

import querki.api.ThingFunctions
import querki.data.{PropInfo, SpaceProps, ThingInfo}
import querki.display.{AfterLoading, ButtonGadget, ButtonKind, Gadget, QText, WrapperDiv}
import querki.display.rx.{RxAttr, RxDiv, RxSelect}

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
    lazy val addButton = new ButtonGadget(ButtonKind.Info, RxAttr("disabled", Rx{ selectedProperty().isEmpty }), "Add")({
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
            new ButtonGadget(ButtonKind.Normal, "Cancel")({ mainDiv.replaceContents(initButton.rendered, true) })
          ),
          hr,
          p(new ButtonGadget(ButtonKind.Info, "Create a new Property instead")({}))
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
