package querki.editing

import org.scalajs.dom.{raw => dom}
import org.querki.jquery._
import scalatags.JsDom.all._
import rx._
import org.querki.gadgets._

import querki.data.{TID => _TID, _}
import querki.display.{ButtonGadget, QuerkiUIUtils}
import querki.display.rx._
import querki.globals._
import querki.util.ScalatagUtils.FSeq

class AddExistingPropertyGadget(page:ModelDesignerPage, thing:ThingInfo, mainSpaceProps:SpaceProps, apg:AddPropertyGadget)
  (implicit val ecology:Ecology, ctx:Ctx.Owner)
  extends Gadget[dom.HTMLDivElement] with QuerkiUIUtils
{
  val optLabel = attr("label")
  
  def reset() = {
    propSelector.get.setValue("")
  }
  
  override def onInserted() = { propSelector.mapElemNow($(_).focus()) }

  // The add button is only enabled when the selection is non-empty; when pressed, it tells the parent
  // page to add the Property:
  val addButton = GadgetRef[ButtonGadget]
  val propSelector = GadgetRef[RxSelect]
  
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
            filter { prop =>
              if (prop.appliesTo.isEmpty)
                // There's no appliesTo, so it is presumed to apply to all Kinds:
                true
              else
                // Is this Kind in the appliesTo list?
                prop.appliesTo.contains(thing.kind)
            }.
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
  
  lazy val selectedProperty = propSelector.get.selectedTIDOpt
  
  def doRender() = {
    val result = div(cls:="well container col-md-12",
      p("""Choose a property from this list of existing properties, or press "Create a New Property" to do something different."""),
      div(cls:="row",
        div(cls:="col-md-4",
          p(
            propSelector <= RxSelect(propOptions, cls:="form-control", id:="_existingPropSelector")
          ),
          p(
            addButton <= 
              new ButtonGadget(ButtonGadget.Info, disabled := Rx { selectedProperty().isEmpty }, id:="_addExistingProperty", "Add")({ () =>
                page.addProperty(selectedProperty.now.get)
                reset()
              })
          ),
          hr,
          p(new ButtonGadget(ButtonGadget.Info, "Create a new Property instead", id:="_createInstead")({ () =>
            apg.showCreateNew()
          }), apg.cancelButton)
        ),
        div(cls:="col-md-7", 
          new DescriptionDiv(page, propSelector.get.selectedWithTID)
        )
      )
    )
    result
  }
}
