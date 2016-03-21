package querki.editing

import scala.xml.Elem

import scalatags.Text.all.{id => idAttr, _}
import scalatags.Text.TypedTag

import models.{DisplayPropVal, Kind, OID, Property, PropertyBundle, Thing, ThingState, Wikitext}
import Thing._

import querki.api.commonName
import querki.core.{LinkCandidateProvider, QLText}
import querki.ecology._
import querki.globals._
import querki.html.RenderSpecialization._
import querki.identity.User
import querki.ql.{QLCall, QLParam, QLPhrase}
import querki.types._
import querki.util._
import querki.values._

class EditorModule(e:Ecology) extends QuerkiEcot(e) with Editor with querki.core.MethodDefs with ThingEditor {
  import MOIDs._
  
  val Types = initRequires[querki.types.Types]
  val Basic = initRequires[querki.basic.Basic]
  val Logic = initRequires[querki.logic.Logic]
  val Links = initRequires[querki.links.Links]
  
  lazy val AccessControl = interface[querki.security.AccessControl]
  lazy val ApiRegistry = interface[querki.api.ApiRegistry]
  lazy val Apps = interface[querki.apps.Apps]
  lazy val DeriveName = interface[querki.types.DeriveName]
  lazy val HtmlRenderer = interface[querki.html.HtmlRenderer]
  lazy val HtmlUI = interface[querki.html.HtmlUI]
  lazy val PropListMgr = interface[querki.core.PropListManager]
  lazy val QL = interface[querki.ql.QL]
  lazy val SkillLevel = interface[querki.identity.skilllevel.SkillLevel]
  lazy val SpaceOps = interface[querki.spaces.SpaceOps]
  lazy val UserValues = interface[querki.uservalues.UserValues]
  
  lazy val PlainTextType = Basic.PlainTextType
  
  lazy val DisplayTextProp = Basic.DisplayTextProp
  lazy val NameProp = Core.NameProp
  
  override def postInit() = {
    ApiRegistry.registerApiImplFor[EditFunctions, EditFunctionsImpl](SpaceOps.spaceRegion)
  }
  
  def getInstanceEditor(thing:PropertyBundle, context:QLContext, currentValue:Option[DisplayPropVal] = None):Future[Wikitext] = {
    instanceEditorForThing(thing, context.next(thing.thisAsQValue).copy(currentValue = currentValue)(context.state, ecology), None)
  }
  
  /***********************************************
   * PROPERTIES
   ***********************************************/
    
  lazy val PlaceholderTextProp = new SystemProperty(PlaceholderTextOID, PlainTextType, Optional,
      toProps(
        setName("Placeholder Text"),
        AppliesToKindProp(Kind.Property),
        Basic.DeprecatedProp(true),
        Summary("Placeholder text for input boxes"),
        Details("""In Text Properties, it is often helpful to have a prompt that displays inside the input
            |field until the user begins to type something there. If the Property has a Placeholder Text, that
            |will be displayed in grey when the input is first shown.""".stripMargin)
        ))
  
  lazy val PromptProp = new SystemProperty(PromptOID, PlainTextType, Optional,
      toProps(
        setName("Prompt"),
        AppliesToKindProp(Kind.Property),
        Summary("Prompt to use in the Editor"),
        Details("""In the Editor, Properties are usually displayed with their Name. If you want to show something
            |other than the Name, set the Prompt Property to say what you would like to show instead.""".stripMargin)
        ))
	
	// TODO: this should really only allow the properties that are defined on this Model:
    // TODO: this has broadened in scope, and really doesn't belong in Editor any more:
	lazy val InstanceProps = new SystemProperty(InstanceEditPropsOID, LinkType, QList,
	    toProps(
	      setName(commonName(_.editing.instancePropsProp)),
	      Links.LinkAllowAppsProp(true),
	      Links.LinkKindProp(Kind.Property),
	      Summary("Which Properties are relevant for Instances of this Model?"),
	      Details("""This Property defines which of the Properties on this Model will show in
          |the editor for its Instances, and in what order.
          |
          |You usually should not need to set this manually; it is changed by dragging-and-dropping
          |Properties in the Model Editor.""".stripMargin))) with LinkCandidateProvider
	{
	  def getLinkCandidates(state:SpaceState, currentValue:DisplayPropVal):Seq[Thing] = {
	    currentValue.on match {
	      case Some(thing) => {
	        // We're applying this to some actual thing, so list its Properties as options:
	        thing.allProps(state).toSeq.sortBy(_.displayName)
	      }
	      case _ => Seq.empty
	    }
	  }
	}

  lazy val InstanceEditViewProp = new SystemProperty(InstanceEditViewOID, LargeTextType, ExactlyOne,
      toProps(
        setName("Instance Edit View"),
        Summary("Defines the Edit View for Instances of this Model"),
        Details("""Sometimes, you want to customize your editing experience -- to make things easier or more
            |efficient, or prettier for your users. Regardless of the reason, this Property gives you complete
            |control.
            |
            |This is an arbitrary Large Text, which is shown whenever you say
            |[[_code(""[[My Instance._edit]]"")]]
            |The contents are up to you, but it should usually contain _edit functions for each Property you
            |want to be editable.""".stripMargin)))
  
  lazy val EditWidthProp = new SystemProperty(EditWidthPropOID, IntType, ExactlyOne,
      toProps(
        setName("Edit Width"),
        Types.MinIntValueProp(1),
        Types.MaxIntValueProp(12),
        Summary("Lets you control how wide a Property's edit control is, in the Edit View"),
        Details("""This is width in Bootstrap span terms -- a number from 1 (narrow) to 12 (full width).
          |
          |You do not have to set this -- if you leave it alone, each Type has a default width. But you will
          |sometimes find that you prefer to have your Edit controls narrower then the default, in order to
          |get a good-looking Editor for a Model. Set this to a number between 1 and 12 to do that.""".stripMargin)))
  
  lazy val NotEditableProp = new SystemProperty(NotEditableOID, YesNoType, ExactlyOne,
    toProps(
      setName("_Not Editable Property"),
      setInternal,
      Summary("Set on a Property that should not show up in the Editor.")))

  abstract class EditMethodBase(id:OID, pf:PropMap) extends InternalMethod(id, pf)
  {
    def specialization(mainContext:QLContext, mainThing:PropertyBundle, 
      partialContext:QLContext, prop:Property[_,_],
      params:Option[Seq[QLParam]]):Set[RenderSpecialization] = Set(FromEditFunction)
  
    def cantEditFallback(inv:Invocation):Future[QValue]
    
    def canEdit(context:QLContext, requester:User, thing:PropertyBundle, prop:Property[_,_]):Boolean = {
      thing match {
        case t:Thing => {
          implicit val state = context.state
          if (UserValues.isUserValueProp(prop))
            AccessControl.hasPermission(UserValues.UserValuePermission, state, requester, t.id)
          else
            AccessControl.canEdit(context.state, requester, t.id)
        }
        // TODO: this isn't right. It should return true iff the requester can edit the Thing
        // this bundle is contained in. Hmm...
        case _ => true
      }
    }
  
    def applyToPropAndThing(inv:Invocation, elemContext:QLContext, mainThing:PropertyBundle, 
      definingContext:QLContext, prop:Property[_,_],
      params:Option[Seq[QLParam]]):QFut =
    {
      elemContext.request.requester match {
        case Some(requester) if (canEdit(elemContext, requester, mainThing, prop)) => {
          val currentValue = mainThing.getDisplayPropVal(prop)(elemContext.state).copy(cont = elemContext.currentValue)
	        // TODO: conceptually, this is a bit off -- the rendering style shouldn't be hard-coded here. We
  	      // probably need to have the Context contain the desire to render in HTML, and delegate to the
  	      // HTML renderer indirectly. In other words, the Context should know the renderer to use, and pass
  	      // that into here:
          for {
  	        inputControl <- HtmlRenderer.renderPropertyInput(elemContext, prop, currentValue, 
  	          specialization(elemContext, mainThing, definingContext, prop, params))
          }
  	        yield HtmlUI.HtmlValue(inputControl)    
        }
        case _ => cantEditFallback(inv)
      }
    }
  
    override def qlApply(inv:Invocation):QFut = {
      val mainContext = inv.context
      val partialContextOpt = inv.definingContext
      val params = inv.paramsOpt
      
      def editThing(thing:Thing, context:QLContext)(implicit state:SpaceState):QFut = {
        if (thing.ifSet(Core.IsModelProp)) {
          val allInstances = state.descendants(thing.id, false, true).toSeq.sortBy(_.displayName)
          for {
            p <- inv.processParamFirstAs(0, IntType)
            page = p - 1
            pageSize <- inv.processParamFirstAs(1, IntType)
            startAt = pageSize * page
            instance <- inv.iter(allInstances.drop(startAt).take(pageSize))
            wikitext <- inv.fut(instanceEditorForThing(instance, instance.thisAsContext(context.request, state, ecology), Some(inv)))
          }
            yield ExactlyOne(QL.ParsedTextType(wikitext))
        } else {
          instanceEditorForThing(thing, context, Some(inv)).map(QL.WikitextValue)
        }
      }
      
      partialContextOpt match {
        case Some(definingContext) => {
          // [[THINGS -> PROP._edit]] -- there is a PROP specified
          for {
            prop <- inv.definingContextAsProperty
            (bundle, elemContext) <- inv.contextBundlesAndContexts
            result <- inv.fut(applyToPropAndThing(inv, elemContext, bundle, definingContext, prop, params))
          }
            yield result 
        }
        
        case None => {
          // [[THINGS -> _edit]] -- there is no PROP specified
          for {
            thing <- inv.contextAllThings
            result <- inv.fut(editThing(thing, inv.context)(inv.state))
          }
            yield result
        }
      } 
    }
  }
  
  /**
   * This is a place to stick weird, special filters.
   */
  def specialModelFilter(model:PropertyBundle, prop:Property[_,_])(implicit state:SpaceState):Boolean = {
    // We display Default View iff it is defined locally on this Thing, or it is *not*
    // defined for the Model.
    // TBD: this is kind of a weird hack. Is it peculiar to Default View, or is there
    // a general concept here?
    if (prop == DisplayTextProp) {
      !model.localProp(DisplayTextProp).isDefined
    } else
      true
  }
  
  /**
   * TODO: unify this with the similar function in ThingEditor. This one is Model-oriented, though, where that is
   * Instance-oriented.
   */
  def instancePropsForModel(model:PropertyBundle, state:SpaceState):Seq[Property[_,_]] = {
    implicit val s = state
    
    def fromIds(propIds:Iterable[OID]):Iterable[Property[_,_]] = propIds.map(state.prop(_)).flatten
    
    val propsOpt = 
      for {
        propsToEdit <- model.getPropOpt(InstanceProps)
        propIds = propsToEdit.v.rawList(LinkType)
      }
        yield fromIds(propIds)
        
    propsOpt.map(_.toSeq).getOrElse(fromIds(model.props.keys).filter(specialModelFilter(model, _)).toSeq.sortBy(_.displayName))
  }
    
  lazy val editMethod = new EditMethodBase(EditMethodOID, 
    toProps(
      setName("_edit"),
      Summary("Puts an editor for the specified Property into the page"),
      Details("""Sometimes, you want to make it easy to edit a Thing, without having to go into the Editor
          |page. For instance, there may be a single button, or a few fields, that should be more easily editable
          |directly when you are looking at the Thing. That is when you use _edit.
          |
          |Use it like this:
          |    THING -> PROPERTY._edit
          |This means "put an edit control for the PROPERTY on THING right here".
          |
          |There isn't yet a way to say what particular *kind* of edit control is used -- there is a default control
          |depending on PROPERTY. For instance, if it is a Large Text Property, a big resizeable text input will be
          |shown. If it is an Optional Yes or No Property, a trio of Yes/Maybe/No buttons will be displayed. (Later,
          |we will undoubtedly add ways to control this more precisely.)
          |
          |The edit control will display the current contents of PROPERTY when it is shown. Changes take place immediately,
          |with no "save" button or anything like that. (Later, we plan to make a Save button optional, but that still
          |needs to be designed.)
          |
          |If the current user isn't allowed to edit this Thing, _edit instead displays the ordinary, rendered value of
          |the Property. If you want to do something else in this case, use [[_editOrElse._self]] instead.""".stripMargin),
      AppliesToKindProp(Kind.Property)
    )) 
  {
    def cantEditFallback(inv:Invocation):QFut = {
      // This user isn't allowed to edit, so simply render the property in its default form.
      // For more control, user _editOrElse instead.
      for {
        prop <- inv.definingContextAsProperty
        result <- inv.fut(prop.qlApply(inv))
      }
        yield result
    }  
  }
  
  lazy val editOrElseMethod = new EditMethodBase(EditOrElseMethodOID, 
    toProps(
      setName("_editOrElse"),
      Summary("Like [[_edit._self]], but you can say what to show if the user can't edit this Property"),
      Details("""See [[_edit._self]] for the full details of how edit control works. This is just like that,
          |but with an additional parameter:
          |    THING -> PROPERTY._editOrElse(FALLBACK)
          |If the current user isn't allowed to edit THING, then FALLBACK is produced instead.""".stripMargin),
      AppliesToKindProp(Kind.Property)
    )) 
  {
    def cantEditFallback(inv:Invocation):Future[QValue] = {
      val context = inv.context
      val paramsOpt = inv.paramsOpt
      
      // This user isn't allowed to edit, so display the fallback
      paramsOpt match {
        case Some(params) if (params.length > 0) => {
          context.parser.get.processPhrase(params(0).firstOps, context).map(_.value)
        }
        case _ => Future.successful(QL.WarningValue("_editOrElse requires a parameter"))
      }
    }  
  }

  /**
   * This is probably badly factored -- in the long run, I suspect this should actually be a param to _edit instead.
   * But this will do to start.
   * 
   * TODO: this is weirdly incestuous with HtmlRenderer. Think about how the factoring should really work.
   */
  lazy val editAsPicklistMethod = new EditMethodBase(EditAsPickListOID, 
    toProps(
      setName("_editAsPickList"),
      Summary("Edits a Tag or Link Set as a Pick List"),
      Details("""This is broadly similar to [[_edit._self]], but displays in a way that is sometimes more useful.
          |
          |To use _editAsPickList, your set must have Restrict to Model set. This displays all known instances of that Model
          |as a checklist, and allows you to decide what is in or out simply by checking things in the list.
          |
          |This function will likely be replaced with a variant of _edit, so don't get too attached to it.""".stripMargin),
      AppliesToKindProp(Kind.Property)
    )) 
  {
    // TODO: this is stolen directly from _edit, and should probably be refactored:
    def cantEditFallback(inv:Invocation):QFut = {
      // This user isn't allowed to edit, so simply render the property in its default form.
      // For more control, user _editOrElse instead.
      for {
        prop <- inv.definingContextAsProperty
        result <- inv.fut(prop.qlApply(inv))
      }
        yield result
    }  
    
    override def specialization(mainContext:QLContext, mainThing:PropertyBundle, 
      partialContext:QLContext, prop:Property[_,_],
      paramsOpt:Option[Seq[QLParam]]):Set[RenderSpecialization] = 
    {
      // This is basically saying "if there is one parameter, and it is the token 'withAdd'"
      // TODO: all of this should go behind a better-built parameter wrapper.
      val hasAddOpt = for (
        params <- paramsOpt;
        if (params.length > 0);
        param = params(0);
        QLCall(addName, _, _, _) = param.firstOps(0);
        if (addName.name.toLowerCase() == "withadd")
          )
        yield true
        
      hasAddOpt.map(_ => Set(PickList, WithAdd, FromEditFunction)).getOrElse(Set(PickList, FromEditFunction))
    }
  }

	// TODO: this code is pretty damned Bootstrap-specific, which by definition is too HTML-specific. We should probably
	// replace it with something that is much more neutral -- simple label/control styles -- and have client-side code
	// that rewrites it appropriately for the UI in use.
	lazy val FormLineMethod = new InternalMethod(FormLineMethodOID,
	    toProps(
	      setName("_formLine"),
	      Summary("Display a label/control pair for an input form"),
	      Details("""_formLine(LABEL,CONTROL) displays the LABEL/CONTROL pair as a standard full-width line. 
	          |
	          |This is mainly for input forms, and is pretty persnickety at this point. It is not recommend for general use yet.""".stripMargin)))
	{
	  override def qlApply(inv:Invocation):QFut = {
	    inv.paramsOpt match {
	      case Some(params) if (params.length == 2) => {
	        val context = inv.definingContext.get
          for {
            label <- context.parser.get.processPhrase(params(0).firstOps, context).map(_.value)
            labelWiki <- label.wikify(context)
            control <- context.parser.get.processPhrase(params(1).firstOps, context).map(_.value)
            controlWiki <- control.wikify(context)
          }
	          yield QL.WikitextValue(
  	          Wikitext("\n{{form-horizontal:\n{{control-group:\n{{control-label:\n") +
  	          labelWiki +
  	          Wikitext("\n}}\n{{controls:\n") +
  	          controlWiki +
  	          Wikitext("\n}}\n}}\n}}\n"))
	      }
	      case _ => QL.WarningFut("_formLine requires two parameters")
	    }
	  }
	}
  
  lazy val CheckListMethod = new InternalMethod(CheckListOID,
    toProps(
      setName("_checkList"),
      setInternal,
      Summary("""Display a checklist of items to add or remove from a Set."""),
      Signature(
        expected = Some(Seq(LinkType), "The items to choose from."),
        defining = Some(true, Seq(LinkType), "The Property that actually contains the checklist, which should be a Set of Things"),
        reqs = Seq(
          ("on", LinkType, "The Thing that contains the Set.")
        ),
        opts = Seq(
          ("selectedOnly", YesNoType, ExactlyOne(Logic.False), "If True, only items currently in the Set will be displayed."),
          ("display", AnyType, Core.QNone, "How to display each item.")
        ),
        returns = (AnyType, "The Check List, ready to display.")
      ),
      Details("""Sometimes, you want to be able to take a Set, and use it as a checklist. The `_checkList`
        |function is designed for that.
        |
        |For example, say that you have a Shopping List Space. There is a Shopping Item Model, with an Instance
        |for each item that you sometimes shop for, and a Shops Tag Set saying where it can be found.
        |You have a Thing named "Shopping List", with a Property "Contents", which is a Set of Things.
        |You would display the whole shopping list, as a checklist, like this:
        |```
        |\[[Shopping Item._instances -> Contents.checkList(on = Shopping List)\]]
        |```
        |That is, the actual list is found in Shopping List.Contents, and you are choosing from
        |all Instances of Shopping Item. This lets you easily add Items to the list.
        |
        |Sometimes, you only want to list the items that are actually checked-off. For example, when
        |you go shopping, you just want to be checking things *off* the list. So you would say:
        |```
        |\[[Shopping Item._instances -> Contents._checkList(on = Shopping List, selectedOnly = true)\]]
        |```
        |By using `selectedOnly` like this, it will only display the currently-selected Items, so
        |you can check them off.
        |
        |You can filter which items show on the list. For instance, you could show a list of just the
        |Items to look for at Acme like this:
        |```
        |\[[Shopping Item._instances 
        |  -> _filter(Shops -> _contains(Acme))
        |  -> Contents._checkList(on = Shopping List, selectedOnly = true)\]]
        |```
        |This way, you can easily display customized checklists for particular situations.
        |
        |By default, _checkList will display show each item's Name. But you can customize this using
        |the `display` parameter. If provided, this will be applied to every item, and the result is
        |what will be shown.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QFut = {
      implicit val state = inv.state
      
      for {
        selectedOnly <- inv.processAs("selectedOnly", YesNoType)
        setOID <- inv.processAs("on", LinkType)
        setThing <- inv.opt(state.anything(setOID))
        setProp <- inv.definingContextAsPropertyOf(LinkType)
        currentPV <- inv.opt(setThing.getPropOpt(setProp), Some(new PublicException("Edit.checklist.propMissing", setThing.displayName, setProp.displayName)))
        currentSet = currentPV.rawList
        itemStr <- inv.fut(items(inv, currentSet, selectedOnly))
      }
        yield 
          HtmlUI.HtmlValue(
            form(
              cls:="_checklist",
              data.thing:=setThing.id.toThingId.toString,
              data.prop:=setProp.id.toThingId.toString,
              ul(
                cls:="_listContent",
                raw(itemStr)
              ))) 
    }
    
    def items(inv:Invocation, currentSet:List[OID], selectedOnly:Boolean):Future[String] = {
      implicit val state = inv.state
      implicit val rc = inv.context.request
      
      val itemInv = for {  
        elemCtx <- inv.contextElements
        t <- inv.contextAllThings(elemCtx)
        selected = currentSet.contains(t.id)
        // If we're in selectedOnly, screen out items that aren't in the set:
        if (selected || !selectedOnly)
        displayOpt <- inv.processAsOpt("display", QL.ParsedTextType, elemCtx)
        display <- inv.fut(displayOpt.map(d => fut(d.display)).getOrElse(t.nameOrComputed))
      }
        yield 
          // For each element...
          li(
            // ... show the checkbox...
            input(
              cls:="_checkOption", 
              value:=t.id.toThingId.toString,
              tpe:="checkbox",
              if (selected) checked:="checked"),
            " ",
            // ... and the display content
            div(cls:="_pickName", raw(display.toString))
          ).toString
          
      // Turn them all into one String, to return to the main function:
      itemInv.get.map(_.toSeq.reduce(_ + _))
    }
  }
  
  override lazy val props = Seq(
    PlaceholderTextProp,
    PromptProp,
    InstanceProps,
    InstanceEditViewProp,
    editMethod,
    editOrElseMethod,
    editAsPicklistMethod,
    EditWidthProp,
    FormLineMethod,
    NotEditableProp,
    CheckListMethod
  )
}