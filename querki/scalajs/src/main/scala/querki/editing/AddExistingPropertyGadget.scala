package querki.editing

import org.scalajs.dom.{raw => dom}
import org.querki.jquery._
import scalatags.JsDom.all._
import rx._

import querki.data.{TID => _TID, _}
import querki.display.{ButtonGadget, Gadget}
import querki.display.rx.{RxAttr, RxGadget, RxSelect}
import querki.globals._

class AddExistingPropertyGadget(page:ModelDesignerPage, thing:ThingInfo, mainSpaceProps:SpaceProps, apg:AddPropertyGadget)(implicit val ecology:Ecology)
  extends Gadget[dom.HTMLDivElement] 
{
  
  val optLabel = "label".attr
  
  def reset() = {
    propSelector.setValue("")
  }

  // The add button is only enabled when the selection is non-empty; when pressed, it tells the parent
  // page to add the Property:
  val addButton = RxGadget[ButtonGadget]
  val propSelector = RxGadget[RxSelect]
  
  lazy val existingPropIds = Rx { 
    page.instancePropSection().propIds() ++
    (if (page.modelPropSection.exists)
       page.modelPropSection().propIds()
     else
       Set.empty)
  }
  
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
            sortBy(_.linkName)
          
        if (props.isEmpty)
          None
        else
          Some(optgroup(optLabel:=s"$prefix Properties in ${spaceProps.displayName}",
            props.map { prop => option(value:=prop, prop.linkName) }
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
  
  lazy val selectedProperty = propSelector.selectedTIDOpt
  
  def doRender() = {
    val result = div(cls:="well container col-md-12",
      p("""Choose a property from this list of existing properties, or press "Create a New Property" to do something different."""),
      div(cls:="row",
        div(cls:="col-md-4",
          p(
            propSelector <= RxSelect(propOptions, cls:="form-control")
          ),
          p(
            addButton <= 
              new ButtonGadget(ButtonGadget.Info, RxAttr("disabled", Rx{ selectedProperty().isEmpty }), "Add")({ () =>
                page.addProperty(selectedProperty().get)
                reset()
              })
          ),
          hr,
          p(new ButtonGadget(ButtonGadget.Info, "Create a new Property instead")({ () => apg.mainDiv.replaceContents(apg.createNew.rendered, true) }), apg.cancelButton)
        ),
        div(cls:="col-md-7", 
          new DescriptionDiv(page, propSelector.selectedWithTID)
        )
      )
    )
    result
  }
}