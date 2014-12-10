package querki.editing

import org.scalajs.dom

import autowire._
import rx._
import rx.ops._
import scalatags.JsDom.all._

import querki.globals._

import querki.api.ThingFunctions
import querki.data._
import querki.display.{AfterLoading, ButtonGadget, ButtonKind, Gadget, QText, WrapperDiv}
import querki.display.input.TextInputGadget
import querki.display.rx.{ButtonInfo, RxAttr, RxButtonGroup, RxDiv, RxSelect}

class AddPropertyGadget(page:ModelDesignerPage, thing:ThingInfo)(implicit val ecology:Ecology) extends Gadget[dom.HTMLDivElement] with EcologyMember {
  
  val optLabel = "label".attr
  
  lazy val Client = interface[querki.client.Client]
  lazy val DataAccess = interface[querki.data.DataAccess]
  
  lazy val mainDiv = (new WrapperDiv).initialContent(initButton)
  
  lazy val initButton:ButtonGadget = new ButtonGadget(ButtonKind.Info, icon("plus"), " Add a Property")({
    mainDiv.replaceContents(addExisting.rendered, true)
  })
  
  lazy val cancelButton = new ButtonGadget(ButtonKind.Normal, "Cancel")({ 
    mainDiv.replaceContents(initButton.rendered, true) 
  })
  
  // Note that we pro-actively begin loading this immediately. It's one of the more common operations for the
  // Model Designer, and we want quick response.
  val allPropsFut = DataAccess.getAllProps()
  val stdInfoFut = DataAccess.standardInfo
  lazy val allTypesFut = DataAccess.getAllTypes()
  
  /**
   * This watches an RxSelect full of Things, and produces the div describing the currently-selected Thing.
   * 
   * @param selector The selected() reactive of the RxSelect. We pass in this instead of the RxSelect itself
   *   so that you can orElse multiple RxSelects and feed the union into here.
   */
  class DescriptionDiv(selector:Rx[Option[(RxSelect, String)]]) {    
    val emptyDescription = span(raw("&nbsp;"))
    val selectedDescriptionObs = Obs(selector, skipInitial=true) {
      selector() match {
        case Some((sel, oid)) => {
          val name = sel.selectedText()
          val fut = for {
            stdInfo <- stdInfoFut
            summaryOpt <- Client[ThingFunctions].getPropertyDisplay(oid, stdInfo.summaryPropId).call()
            detailsOpt <- Client[ThingFunctions].getPropertyDisplay(oid, stdInfo.detailsPropId).call()
          }
            yield
              // ... build the display of the Property info...
              div(
                b(name),
                summaryOpt.map(summary => i(new QText(summary))),
                detailsOpt.map(details => new QText(details))
              )
            
          fut.foreach { desc => selectionDescription() = desc }
        }
        
        case None => selectionDescription() = emptyDescription
      }
    }
    val selectionDescription = Var[Gadget[dom.Element]](emptyDescription)
    
    val descriptionDiv = RxDiv(Rx {Seq(selectionDescription())})
  }
  
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
    lazy val selectedPropertyDescription = new DescriptionDiv(propSelector.selected)
    lazy val propertyDescriptionDiv = selectedPropertyDescription.descriptionDiv
    
    def doRender() = {
      val result = div(cls:="well container span12",
        p(i(cls:="fa fa-spinner fa-spin"), """Choose a property from this list of existing properties, or press "Create a New Property" to do something different."""),
        div(cls:="row-fluid",
          div(cls:="span4",
            p(
              propSelector
            ),
            p(
              addButton
            ),
            hr,
            p(new ButtonGadget(ButtonKind.Info, "Create a new Property instead")({ mainDiv.replaceContents(createNew.rendered, true) }), cancelButton)
          ),
          div(cls:="span7", propertyDescriptionDiv)
        )
      )
      result
    }
  }

/*
              <div class="span6">
                <div class="row-fluid">
		            <input type="text" id="_newPropName" class="span6" placeholder="Name (required)...">
		            <select id="_newPropType" class="span6">
		                <option value="">Choose the Property's type (required)...</option>
		                @for(aType <- space.allTypes.values.
		                       filterNot(_.ifSet(Core.InternalProp)(space)).
		                       filterNot(_.ifSet(Basic.DeprecatedProp)(space)).
		                       filterNot(typ => typ.isInstanceOf[querki.types.ModelTypeBase] && !typ.ifSet(Basic.ExplicitProp)(space)).
		                       toSeq.sortBy(_.displayName)) {
		                  <option value="@aType.id.toString" @collForType(aType)>@aType.displayName</option>
		                }
		                <option value=@createTypeIndicator>Based on a Model</option>
		            </select>
		        </div>
		        <div class="row-fluid">
				    <div id="_newPropColl" class="btn-group span6" data-toggle="buttons-radio" style="margin-left: 0px;">
					    <button id="_newPropOne" type="button" class="btn btn-primary active" value="@{ExactlyOne.id.toString}">Exactly One</button>
					    <button id="_newPropOptional" type="button" class="btn btn-primary" value="@{Optional.id.toString}">Optional</button>
					    <button id="_newPropList" type="button" class="btn btn-primary" value="@{QList.id.toString}">List</button>
					    <button id="_newPropSet" type="button" class="btn btn-primary" value="@{QSet.id.toString}">Set</button>
					</div>
		            <select id="_newPropLinkModel" style="display:none;" class="span6">
		                <option value="">Link to which Model? (optional)...</option>
		                @for(aModel <- space.allModels.toSeq.sortBy(_.displayName)) {
		                  <option value="@aModel.id.toString">@aModel.displayName</option>
		                }
		            </select>
		            <select id="_newTypeModel" style="display:none;" class="span6">
		                <option value="">Based on which Model?...</option>
		                @for(aModel <- space.models.toSeq.sortBy(_.displayName).filter(_.hasProp(Editor.InstanceProps)(space))) {
		                  <option value="@aModel.id.toThingId">@aModel.displayName</option>
		                }
		            </select>
		        </div>
	  		  </div>
			  <div class="span6">
			    <p id="_typeInfo" style="height:250px; overflow:auto">&nbsp;</p>
			  </div>
 */
  
  class CreateNewPropertyGadget(typeInfo:AllTypeInfo) extends Gadget[dom.HTMLDivElement] {
    
    lazy val nameInput = new TextInputGadget(Seq("span6"), placeholder:="Name (required)...")
    
    // TODO: should the Collections simply come from the global info instead of typeInfo? They aren't changeable yet.
    lazy val collButtons =
      typeInfo.collections.headOption.map { coll => ButtonInfo(coll.oid, coll.name, true) } ++
      typeInfo.collections.tail.map { coll => ButtonInfo(coll.oid, coll.name) }
    lazy val collSelector = new RxButtonGroup(Var(collButtons.toSeq))
    
    val advTypeOptions = Var({
      val typeOpts = typeInfo.advancedTypes.sortBy(_.name).map(typ => option(value:=typ.oid, typ.name))
      option(value:="", "Choose a Type...") +: typeOpts
    })
    val typeSelector = new RxSelect(advTypeOptions, cls:="span5")
    
    val modelOptions = Var({
      val modelOpts = typeInfo.models.sortBy(_.displayName).map(model => option(value:=model.oid, model.displayName))
      option(value:="", "Base it on a Model...") +: modelOpts
    })
    val modelSelector = new RxSelect(modelOptions, cls:="span5")

    // You choose *either* a Type or a Model; when you set one, we unset the other:
    val modelClearer = Obs(typeSelector.selectedValOpt) {
      typeSelector.selectedValOpt().map(_ => modelSelector.setValue(""))
    }
    val typeClearer = Obs(modelSelector.selectedValOpt) {
      modelSelector.selectedValOpt().map(_ => typeSelector.setValue(""))
    }
    
    // The chosen basis is *either* a Model or a Type. selected() combines the currently-chosen value and its
    // RxSelect:
    lazy val selectedBasis = Rx { modelSelector.selected() orElse typeSelector.selected() }
    lazy val selectedBasisDescription = new DescriptionDiv(selectedBasis)
    lazy val selectedBasisDescriptionDiv = selectedBasisDescription.descriptionDiv
    
    def doRender() =
      div(cls:="well container span12",
        p(i(cls:="fa fa-spinner fa-spin"), """Describe the new property to create, or press "Add an Existing Property" to use one that already exists."""),
        div(cls:="row-fluid",
          div(cls:="span6",
            div(cls:="row-fluid",
              nameInput
            ),
            div(cls:="row-fluid",
              collSelector
            ),
            div(cls:="row-fluid",
              typeSelector, " or ", modelSelector
            )
          ),
          div(cls:="span6", selectedBasisDescriptionDiv)
        ),
        p(cls:="offset1"
//          addButton,
        ),
        hr,
        p(new ButtonGadget(ButtonKind.Info, "Add an Existing Property")({ mainDiv.replaceContents(addExisting.rendered, true) }), cancelButton)
      )
  }
  
  lazy val addExisting = AfterLoading(allPropsFut) { spaceProps => 
    new AddExistingPropertyGadget(spaceProps)
  }
  
  lazy val createNew = AfterLoading(allTypesFut) { allTypes =>
    new CreateNewPropertyGadget(allTypes)
  }
  
  def doRender() = {
    div(mainDiv)
  }
}
