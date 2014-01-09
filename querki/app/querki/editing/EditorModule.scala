package querki.editing

import models.{Kind, OID, Property, Thing, ThingState, Wikitext}
import models.Property._
import models.Thing.{PropFetcher, setName, toProps}

import models.system.{SingleContextMethod, SystemProperty}
import models.system.{ExactlyOne, QList}
import models.system.{IntType, LargeTextType, LinkType, QLText}
import models.system.{AppliesToKindProp, DisplayTextProp, InstanceEditPropsProp}
import models.system.OIDs.sysId

import ql.{QLCall, QLParser, QLPhrase}

import querki.html.RenderSpecialization._

import querki.ecology._

import querki.types._
import querki.util._
import querki.values._

object MOIDs extends EcotIds(13) {
  // Previously in System
  val EditMethodOID = sysId(42)
  val FormLineMethodOID = sysId(81) 
  val EditOrElseMethodOID = sysId(82)
    
  val EditAsPickListOID = moid(1)
  val InstanceEditViewOID = moid(2)
  val EditWidthPropOID = moid(3)
}

class EditorModule(e:Ecology) extends QuerkiEcot(e) {
  import MOIDs._
  
  val Types = initRequires[querki.types.Types]
  
  lazy val Conventions = interface[querki.conventions.Conventions]
  lazy val SkillLevel = interface[querki.identity.skilllevel.SkillLevel]
  lazy val PropListMgr = interface[querki.core.PropListManager]
  lazy val Core = interface[querki.core.Core]
  lazy val HtmlRenderer = interface[querki.html.HtmlRenderer]
  
  /***********************************************
   * PROPERTIES
   ***********************************************/
  
  lazy val instanceEditViewProp = new SystemProperty(InstanceEditViewOID, LargeTextType, ExactlyOne,
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
  
  lazy val editWidthProp = new SystemProperty(EditWidthPropOID, IntType, ExactlyOne,
      toProps(
        setName("Edit Width"),
        Types.MinIntValueProp(1),
        Types.MaxIntValueProp(12),
        Summary("Lets you control how wide a Property's edit control is, in the Edit View"),
        Details("""This is width in Bootstrap span terms -- a number from 1 (narrow) to 12 (full width).""".stripMargin)))

  abstract class EditMethodBase(id:OID, pf:PropFetcher) extends SingleContextMethod(id, pf)
  {
    def specialization(mainContext:QLContext, mainThing:Thing, 
      partialContext:QLContext, prop:Property[_,_],
      params:Option[Seq[QLPhrase]]):Set[RenderSpecialization] = Set(Unspecialized)
  
    def cantEditFallback(mainContext:QLContext, mainThing:Thing, 
      partialContext:QLContext, prop:Property[_,_],
      params:Option[Seq[QLPhrase]]):QValue
  
    def applyToPropAndThing(mainContext:QLContext, mainThing:Thing, 
      partialContext:QLContext, prop:Property[_,_],
      params:Option[Seq[QLPhrase]]):QValue =
    {
      mainContext.request.requester match {
        case Some(requester) if (mainContext.state.canEdit(requester, mainThing.id)) => {
          val currentValue = mainThing.getDisplayPropVal(prop)(mainContext.state)
	      // TODO: conceptually, this is a bit off -- the rendering style shouldn't be hard-coded here. We
  	      // probably need to have the Context contain the desire to render in HTML, and delegate to the
	      // HTML renderer indirectly. In other words, the Context should know the renderer to use, and pass
	      // that into here:
	      val inputControl = HtmlRenderer.renderPropertyInput(mainContext.state, prop, currentValue, 
	          specialization(mainContext, mainThing, partialContext, prop, params))
	      HtmlValue(inputControl)    
        }
        case _ => cantEditFallback(mainContext, mainThing, partialContext, prop, params)
      }
    }
  
    /**
     * How wide (in Bootstrap spans) should the editor control for this Property be?
     * 
     * If the Edit Width property is set on this Property, returns that. Otherwise, returns the
     * preferred width of the Type.
     * 
     * This is gradually going to want to get *much* more sophisticated. But it's a start.
     */
    def editorSpan(prop:Property[_,_])(implicit state:SpaceState):Int = prop.getPropOpt(editWidthProp).flatMap(_.firstOpt).getOrElse(prop.pType.editorSpan(this)) 
    
    /**
     * This wrapper creates the actual layout bits for the default Instance Editor. Note that it is *highly*
     * dependent on the styles defined in main.css!
     */
    private case class EditorPropLayout(prop:Property[_,_])(implicit state:SpaceState) {
      def span = editorSpan(prop)
      def summaryTextOpt = prop.getPropOpt(Conventions.PropSummary).flatMap(_.firstOpt).map(_.text)
      def displayNamePhrase = {
        summaryTextOpt match {
          case Some(summaryText) => s"""[[""${prop.displayName}"" -> _tooltip(""$summaryText"")]]"""
          case None => prop.displayName
        }
      }
      def layout = s"""{{span$span:
      |{{_propTitle: $displayNamePhrase:}}
      |
      |[[${prop.toThingId}._edit]]
      |}}
      |""".stripMargin
    }
    
    private case class EditorRowLayout(props:Seq[EditorPropLayout]) {
      def span = (0 /: props) { (sum, propLayout) => sum + propLayout.span }
      def layout = s"""{{row-fluid:
    		  |${props.map(_.layout).mkString}
              |}}
    		  |""".stripMargin
    }
    
    // This hard-coded number comes from Bootstrap, and is pretty integral to it:
    val maxSpanPerRow = 12
    
    /**
     * This takes the raw list of property layout objects, and breaks it into rows of no more
     * than 12 spans each.
     */
    private def splitRows(propLayouts:Iterable[EditorPropLayout]):Seq[EditorRowLayout] = {
      (Seq(EditorRowLayout(Seq.empty)) /: propLayouts) { (rows, nextProp) =>
        val currentRow = rows.last
        if ((currentRow.span + nextProp.span) > maxSpanPerRow)
          // Need a new row
          rows :+ EditorRowLayout(Seq(nextProp))
        else
          // There is room to fit it into the current row
          rows.take(rows.length - 1) :+ currentRow.copy(currentRow.props :+ nextProp)
      }
    }
    
    /**
     * This is a place to stick weird, special filters.
     */
    def specialFilter(thing:Thing, prop:Property[_,_])(implicit state:SpaceState):Boolean = {
      // We display Default View iff it is defined locally on this Thing, or it is *not*
      // defined for the Model.
      // TBD: this is kind of a weird hack. Is it peculiar to Default View, or is there
      // a general concept here?
      if (prop == DisplayTextProp) {
        if (thing.localProp(DisplayTextProp).isDefined)
          true
        else {
          thing.getModelOpt match {
            case Some(model) => {
              val result = for (
                modelPO <- model.getPropOpt(DisplayTextProp);
                if (!modelPO.isEmpty)
                  )
                yield false
                
              result.getOrElse(true)
            }
            case None => true
          }
        }
      } else
        true
    }
    
    private def propsToEditForThing(thing:Thing, state:SpaceState):Iterable[Property[_,_]] = {
      implicit val s = state
      val result = for (
        propsToEdit <- thing.getPropOpt(InstanceEditPropsProp);
        propIds = propsToEdit.v.rawList(LinkType);
        props = propIds.map(state.prop(_)).flatten    
          )
        yield props

      // Note that the toList here implicitly sorts the PropList, more or less by display name:
      result.getOrElse(PropListMgr.from(thing).toList.map(_._1).filterNot(SkillLevel.isAdvanced(_)).filter(specialFilter(thing, _)))
    }
    
    private def editorLayoutForThing(thing:Thing, state:SpaceState):QLText = {
      implicit val s = state
      thing.getPropOpt(instanceEditViewProp).flatMap(_.v.firstTyped(LargeTextType)) match {
        // There's a predefined Instance Edit View, so use that:
        case Some(editText) => editText
        // Generate the View based on the Thing:
        case None => {
          val layoutPieces = propsToEditForThing(thing, state).map(EditorPropLayout(_))
          val layoutRows = splitRows(layoutPieces)
          // TODO: need to break this into distinct 12-span rows!
          val propsLayout = s"""{{_instanceEditor:
              |${layoutRows.map(_.layout).mkString}
              |}}
              |""".stripMargin
          QLText(propsLayout)
        }
      }
    }
    
    private def instanceEditorForThing(thing:Thing, thingContext:QLContext, params:Option[Seq[QLPhrase]]):Wikitext = {
      implicit val state = thingContext.state
      val editText = editorLayoutForThing(thing, state)
      val parser = new QLParser(editText, thingContext, params)
      parser.process
    }
  
    def fullyApply(mainContext:QLContext, partialContext:QLContext, params:Option[Seq[QLPhrase]]):QValue = {
      applyToIncomingThing(partialContext) { (partialThing, _) =>
        partialThing match {
          case prop:Property[_,_] => {
            applyToIncomingThing(mainContext) { (mainThing, _) =>
              applyToPropAndThing(mainContext, mainThing, partialContext, prop, params)
            }
          }
          
          case thing:ThingState => {
            implicit val state = partialContext.state
            if (thing.ifSet(Core.IsModelProp)) {
              val instances = state.descendants(thing.id, false, true).toSeq.sortBy(_.displayName)
              val wikitexts = instances.map { instance => instanceEditorForThing(instance, instance.thisAsContext(partialContext.request), params) }
              QList.from(wikitexts, ParsedTextType)
            } else {
              WikitextValue(instanceEditorForThing(thing, partialContext, params))
            }
          }
          
          case _ => ErrorValue("The " + displayName + " method can only be used on Properties, Models and Instances")
        } 
      }
    }
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
    def cantEditFallback(mainContext:QLContext, mainThing:Thing, 
      partialContext:QLContext, prop:Property[_,_],
      params:Option[Seq[QLPhrase]]):QValue = {
        // This user isn't allowed to edit, so simply render the property in its default form.
        // For more control, user _editOrElse instead.
        prop.qlApply(mainContext, params)    
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
    def cantEditFallback(mainContext:QLContext, mainThing:Thing, 
      partialContext:QLContext, prop:Property[_,_],
      paramsOpt:Option[Seq[QLPhrase]]):QValue = {
        // This user isn't allowed to edit, so display the fallback
        paramsOpt match {
          case Some(params) if (params.length > 0) => {
            mainContext.parser.get.processPhrase(params(0).ops, mainContext).value
          }
          case _ => WarningValue("_editOrElse requires a parameter")
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
          |To use _editAsPickList, your set must have a Link Model set. This displays all known instances of that Link Model
          |as a checklist, and allows you to decide what is in or out simply by checking things in the list.""".stripMargin),
      AppliesToKindProp(Kind.Property)
    )) 
  {
    // TODO: this is stolen directly from _edit, and should probably be refactored:
    def cantEditFallback(mainContext:QLContext, mainThing:Thing, 
      partialContext:QLContext, prop:Property[_,_],
      params:Option[Seq[QLPhrase]]):QValue = {
        // This user isn't allowed to edit, so simply render the property in its default form.
        // For more control, user _editOrElse instead.
        prop.qlApply(mainContext, params)    
    }  
    
    override def specialization(mainContext:QLContext, mainThing:Thing, 
      partialContext:QLContext, prop:Property[_,_],
      paramsOpt:Option[Seq[QLPhrase]]):Set[RenderSpecialization] = 
    {
      // This is basically saying "if there is one parameter, and it is the token 'withAdd'"
      // TODO: all of this should go behind a better-built parameter wrapper.
      val hasAddOpt = for (
        params <- paramsOpt;
        if (params.length > 0);
        param = params(0);
        QLCall(addName, _, _, _) = param.ops(0);
        if (addName.name.toLowerCase() == "withadd")
          )
        yield true
        
      hasAddOpt.map(_ => Set(PickList, WithAdd)).getOrElse(Set(PickList))
    }
  }

	// TODO: this code is pretty damned Bootstrap-specific, which by definition is too HTML-specific. We should probably
	// replace it with something that is much more neutral -- simple label/control styles -- and have client-side code
	// that rewrites it appropriately for the UI in use.
	lazy val FormLineMethod = new SingleContextMethod(FormLineMethodOID,
	    toProps(
	      setName("_formLine"),
	      Summary("Display a label/control pair for an input form"),
	      Details("""_formLine(LABEL,CONTROL) displays the LABEL/CONTROL pair as a standard full-width line. 
	          |
	          |This is mainly for input forms, and is pretty persnickety at this point. It is not recommend for general use yet.""".stripMargin)))
	{
	  def fullyApply(mainContext:QLContext, partialContext:QLContext, paramsOpt:Option[Seq[QLPhrase]]):QValue = {
	    paramsOpt match {
	      case Some(params) if (params.length == 2) => {
	        val context = partialContext
	        val label = context.parser.get.processPhrase(params(0).ops, context).value
	        val control = context.parser.get.processPhrase(params(1).ops, context).value
	        WikitextValue(
	          Wikitext("\n{{form-horizontal:\n{{control-group:\n{{control-label:\n") +
	          label.wikify(context) +
	          Wikitext("\n}}\n{{controls:\n") +
	          control.wikify(context) +
	          Wikitext("\n}}\n}}\n}}\n"))
	      }
	      case _ => WarningValue("_formLine requires two parameters")
	    }
	  }
	}
  
  override lazy val props = Seq(
    instanceEditViewProp,
    editMethod,
    editOrElseMethod,
    editAsPicklistMethod,
    editWidthProp,
    FormLineMethod
  )
}