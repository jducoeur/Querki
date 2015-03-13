package querki.editing

import org.scalajs.dom.{raw => dom}
import org.querki.jquery._
import autowire._
import rx._
import rx.ops._
import scalatags.JsDom.all._

import querki.globals._

import querki.api.{EditFunctions, ThingFunctions}
import EditFunctions._
import querki.data.{TID => _TID, _}
import querki.display.{AfterLoading, ButtonGadget, ButtonKind, Gadget, QText, WrapperDiv}
import querki.display.input.TextInputGadget
import querki.display.rx.{ButtonInfo, RxAttr, RxButtonGroup, RxDiv, RxSelect, RxText}

class AddPropertyGadget(page:ModelDesignerPage, thing:ThingInfo)(implicit val ecology:Ecology) extends Gadget[dom.HTMLDivElement] with EcologyMember {
  
  val optLabel = "label".attr
  
  lazy val Client = interface[querki.client.Client]
  lazy val DataAccess = interface[querki.data.DataAccess]
  lazy val Editing = interface[Editing]
  
  lazy val mainDiv = (new WrapperDiv).initialContent(initButton)
  
  lazy val initButton:ButtonGadget = new ButtonGadget(ButtonKind.Info, icon("plus"), " Add a Property")({
    mainDiv.replaceContents(addExisting.rendered, true)
  })
  
  lazy val cancelButton = new ButtonGadget(ButtonKind.Normal, "Cancel")({ reset() })
  
  val stdThingFut = DataAccess.standardThings
  def allTypesFut = page.allTypesFut
  def allPropsFut = page.allPropsFut
  
  def reset() = {
    addExistingGadget().map(_.reset())
    createNewGadget().map(_.reset())
    mainDiv.replaceContents(initButton.rendered, true)
  }
  
  class AddExistingPropertyGadget(mainSpaceProps:SpaceProps) extends Gadget[dom.HTMLDivElement] {
    
    def reset() = {
      propSelector.setValue("")
    }

    // The add button is only enabled when the selection is non-empty; when pressed, it tells the parent
    // page to add the Property:
    lazy val addButton = new ButtonGadget(ButtonKind.Info, RxAttr("disabled", Rx{ selectedProperty().isEmpty }), "Add")({
      page.addProperty(selectedProperty().get)
      reset()
    })
    
    lazy val propSelector = RxSelect(propOptions, cls:="form-control")
    
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
    lazy val selectedPropertyDescription = new DescriptionDiv(page, propSelector.selectedWithTID)
    lazy val propertyDescriptionDiv = selectedPropertyDescription.descriptionDiv
    
    def doRender() = {
      val result = div(cls:="well container col-md-12",
        p(i(cls:="fa fa-spinner fa-spin"), """Choose a property from this list of existing properties, or press "Create a New Property" to do something different."""),
        div(cls:="row",
          div(cls:="col-md-4",
            p(
              propSelector
            ),
            p(
              addButton
            ),
            hr,
            p(new ButtonGadget(ButtonKind.Info, "Create a new Property instead")({ mainDiv.replaceContents(createNew.rendered, true) }), cancelButton)
          ),
          div(cls:="col-md-7", propertyDescriptionDiv)
        )
      )
      result
    }
  }
  
  class CreateNewPropertyGadget(typeInfo:AllTypeInfo) extends Gadget[dom.HTMLDivElement] {
    
    def reset() = {
      nameInput.setValue("")
      typeSelector.setValue("")
      modelSelector.setValue("")
    }
    
    lazy val nameInput = new RxText(cls:="col-md-6 form-control", placeholder:="Name (required)...")
    
    // TODO: should the Collections simply come from the global info instead of typeInfo? They aren't changeable yet.
    lazy val collButtons =
      typeInfo.collections.headOption.map { coll => ButtonInfo(coll.oid.underlying, coll.displayName, true) } ++
      typeInfo.collections.tail.map { coll => ButtonInfo(coll.oid.underlying, coll.displayName) }
    lazy val collSelector = new RxButtonGroup(Var(collButtons.toSeq))
    
    val advTypeOptions = Var({
      val typeOpts = typeInfo.advancedTypes.sortBy(_.displayName).map(typ => option(value:=typ, typ.displayName))
      option(value:="", "Choose a Type...") +: typeOpts
    })
    val typeSelector = new RxSelect(advTypeOptions, cls:="form-control")
    
    val modelOptions = Var({
      val modelOpts = typeInfo.models.sortBy(_.displayName).map(model => option(value:=model, model.displayName))
      option(value:="", "Base it on a Model...") +: modelOpts
    })
    val modelSelector = new RxSelect(modelOptions, cls:="form-control")

    // You choose *either* a Type or a Model; when you set one, we unset the other:
    val modelClearer = Obs(typeSelector.selectedValOpt) {
      typeSelector.selectedValOpt().map(_ => modelSelector.setValue(""))
    }
    val typeClearer = Obs(modelSelector.selectedValOpt) {
      modelSelector.selectedValOpt().map(_ => typeSelector.setValue(""))
    }
    
    // The chosen basis is *either* a Model or a Type. selected() combines the currently-chosen value and its
    // RxSelect:
    lazy val selectedBasis = Rx { modelSelector.selectedWithTID() orElse typeSelector.selectedWithTID() }
    lazy val selectedBasisDescription = new DescriptionDiv(page, selectedBasis)
    lazy val selectedBasisDescriptionDiv = selectedBasisDescription.descriptionDiv
    
    // The add button is only enabled when all fields are non-empty; when pressed, it tells the parent
    // page to add the Property:
    lazy val addButton = 
      new ButtonGadget(ButtonKind.Info, 
          RxAttr("disabled", Rx{ nameInput.textOpt().isEmpty || collSelector.selectedTIDOpt().isEmpty || selectedBasis().isEmpty }), 
          "Create")({
        val name = nameInput.textOpt().get
        val coll = collSelector.selectedTIDOpt().get
        val (selector, oid) = selectedBasis().get
        if (selector == modelSelector) {
          // We're creating it based on a Model, so we need to get the Model Type. Note that this is async:
          Client[EditFunctions].getModelType(oid).call().foreach { typeInfo => createProperty(name, coll, typeInfo.oid) }
        } else {
          // We already have a Type
          createProperty(name, coll, oid)
        }
      })
      
    def createProperty(name:String, collId:TID, typeId:TID) = {
      // Technically, we have to wait for the StandardInfo to be available:
      stdThingFut.foreach { stdThings =>
        def mkPV(oid:BasicThingInfo, v:String) = {
          val path = Editing.propPath(oid)
          ChangePropertyValue(path, Seq(v))
        }
        val initProps = Seq(
          mkPV(stdThings.core.nameProp, name),
          mkPV(stdThings.core.collectionProp, collId.underlying),
          mkPV(stdThings.core.typeProp, typeId.underlying)
        )
        Client[EditFunctions].create(stdThings.core.urProp, initProps).call().foreach { propInfo =>
          page.addProperty(propInfo.oid, true)
          reset()
        }
      }
    }
    
    def doRender() =
      div(cls:="well container col-md-12",
        p(i(cls:="fa fa-spinner fa-spin"), """Describe the new property to create, or press "Add an Existing Property" to use one that already exists."""),
        div(cls:="row",
          div(cls:="col-md-6",
            div(cls:="row",
              div(cls:="col-md-12",
                nameInput
              )
            ),
            div(cls:="row",
              div(cls:="col-md-12",
                collSelector
              )
            ),
            div(cls:="row",
              div(cls:="col-md-5", typeSelector), span(cls:="col-md-1", " or "), div(cls:="col-md-5", modelSelector)
            )
          ),
          div(cls:="col-md-6", selectedBasisDescriptionDiv)
        ),
        p(cls:="col-md-offset1",
          addButton
        ),
        hr,
        p(new ButtonGadget(ButtonKind.Info, "Add an Existing Property")({ mainDiv.replaceContents(addExisting.rendered, true) }), cancelButton)
      )
  }
  
  lazy val addExisting = AfterLoading(allPropsFut) { spaceProps => 
    val g = new AddExistingPropertyGadget(spaceProps)
    addExistingGadget() = Some(g)
    g
  }
  // This is a bit boilerplatey, but we're trying not to evaluate addExisting unnecessarily
  // TODO: should we enhance AfterLoading to be able to put the laziness into there
  // explicitly?
  val addExistingGadget = Var[Option[AddExistingPropertyGadget]](None)
  
  lazy val createNew = AfterLoading(allTypesFut) { allTypes =>
    val g = new CreateNewPropertyGadget(allTypes)
    createNewGadget() = Some(g)
    g
  }
  val createNewGadget = Var[Option[CreateNewPropertyGadget]](None)
  
  def doRender() = {
    div(mainDiv)
  }
}
