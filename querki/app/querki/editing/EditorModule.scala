package querki.editing

import scala.xml.Elem

import models.{DisplayPropVal, Kind, OID, Property, Thing, ThingState, Wikitext}

import querki.core.{LinkCandidateProvider, QLText}

import querki.html.RenderSpecialization._

import querki.ecology._
import querki.ql.{QLCall, QLPhrase}

import querki.types._
import querki.util._
import querki.values._

class EditorModule(e:Ecology) extends QuerkiEcot(e) with Editor with querki.core.MethodDefs with ThingEditor {
  import MOIDs._
  
  val Types = initRequires[querki.types.Types]
  val Basic = initRequires[querki.basic.Basic]
  val Links = initRequires[querki.links.Links]
  
  lazy val SkillLevel = interface[querki.identity.skilllevel.SkillLevel]
  lazy val PropListMgr = interface[querki.core.PropListManager]
  lazy val HtmlRenderer = interface[querki.html.HtmlRenderer]
  lazy val HtmlUI = interface[querki.html.HtmlUI]
  lazy val QL = interface[querki.ql.QL]
  lazy val DeriveName = interface[querki.types.DeriveName]
  
  lazy val PlainTextType = Basic.PlainTextType
  
  lazy val DisplayTextProp = Basic.DisplayTextProp
  lazy val NameProp = Core.NameProp
  
  def getInstanceEditor(thing:Thing, rc:RequestContext):Wikitext = {
    instanceEditorForThing(thing, thing.thisAsContext(rc), None)
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
	lazy val InstanceEditPropsProp = new SystemProperty(InstanceEditPropsOID, LinkType, QList,
	    toProps(
	      setName("Properties to edit in Instances"),
	      Links.LinkAllowAppsProp(true),
	      Links.LinkKindProp(Kind.Property),
	      Summary("Which Properties should be edited in Instances of this Model?"),
	      Details("""It is very common to define a bunch of Properties on a Model that you really don't
	          |ever intend to change on the Instances. (In particular, you very often will define the Display
	          |Text on the Model, not on the Instances.) This results in your Instance Editor being cluttered
	          |with lots of Properties that you never, ever use.
	          |
	          |So this Property is a quick-and-easy way to lay out your Instance Editor. It is a List of
	          |Properties that you can define however you like. When you create or edit an Instance of this
	          |Model, it will display exactly those Properties, in that order, which usually makes it
	          |easier for you to write your Instances.
	          |
	          |BUG NOTE: this doesn't immediately register when you've added a Property to the Model, so it
	          |doesn't list the newly-added Property. For now, after you add a Property, save the Model and then
	          |edit it again -- the Property should now show up for you to use.""".stripMargin))) with LinkCandidateProvider
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
	      HtmlUI.HtmlValue(inputControl)    
        }
        case _ => cantEditFallback(mainContext, mainThing, partialContext, prop, params)
      }
    }
  
    def fullyApply(mainContext:QLContext, partialContext:QLContext, params:Option[Seq[QLPhrase]]):QValue = {
      
      // TODO: this belongs in Invocation as a general mechanism:
      def intParam(name:String, default:Int):Int = {
        val rc = mainContext.request
        val strs = rc.queryParam(name)
        if (strs.length > 0) {
          try {
            java.lang.Integer.parseInt(strs.head)
          } catch {
            case _:Throwable => default
          }
        } else
          default
      }
      
      // TODO: For now, we're just going to go with an abstraction break. But figure out how this should work:
      def paginator(rc:controllers.PlayRequestContext, allInstances:Seq[Thing], startAt:Int, pageSize:Int):Wikitext = {
        val req = rc.request
        val reqParams = (req.queryString - "page").map { pair =>
          val (k, v) = pair
          val vs = v.mkString(",")
          s"$k=$vs"
        }.toSeq.mkString("&")
        val baseUri = req.path + "?" + reqParams
        
        def urlForPage(num:Int):String = {
          baseUri + "&page=" + num.toString
        }
        
        // Suggested on this page: http://stackoverflow.com/questions/17944/how-to-round-up-the-result-of-integer-division
        val totalPages = (allInstances.length - 1) / pageSize + 1
        if (totalPages > 1) {
          val curPage = (Math.floor(startAt / pageSize)).toInt + 1
          val leftPage = Math.max(curPage - 3, 0) + 1
          val rightPage = Math.min(curPage + 3, totalPages) + 1
          val range = leftPage until rightPage
          val pages = range.map { pageNum =>
              <li class={ if (pageNum == curPage) "active" else "" }><a href={urlForPage(pageNum)}>{pageNum}</a></li>
          }
          val leftEllipses:Seq[Elem] = { 
            if (leftPage > 1)
              Seq(<li class="disabled"><a href="#">...</a></li>)
            else
              Seq.empty
          }
          val rightEllipses:Seq[Elem] = { 
            if (rightPage <= totalPages)
              Seq(<li class="disabled"><a href="#">...</a></li>)
            else
              Seq.empty
          }
          val xml =
            <div class="pagination pagination-centered">
              <ul>
              <li><a href={urlForPage(1)}>&laquo;</a></li>
              <li><a href={ if (curPage == 1) "#" else urlForPage(curPage - 1) } class={ if (curPage == 1) "disabled" else "" }>&lsaquo;</a></li>
              { leftEllipses }
              { pages }
              { rightEllipses }
              <li><a href={ if (curPage == totalPages) "#" else urlForPage(curPage + 1) } class={ if (curPage == totalPages) "disabled" else "" }>&rsaquo;</a></li>
              <li><a href={urlForPage(totalPages)}>&raquo;</a></li>
              </ul>
            </div>
          HtmlUI.toWikitext(xml)
        } else
          Wikitext("")
      }
      
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
              val allInstances = state.descendants(thing.id, false, true).toSeq.sortBy(_.displayName)
              val page = intParam("page", 1) - 1
              val pageSize = intParam("pageSize", 10)
              val startAt = pageSize * page
              val instances = allInstances.drop(startAt).take(pageSize)
              val wikitexts = 
                instances.map { instance => instanceEditorForThing(instance, instance.thisAsContext(partialContext.request), params) } :+
                createInstanceButton(thing, mainContext)
              Core.listFrom(paginator(partialContext.request.asInstanceOf[controllers.PlayRequestContext], allInstances, startAt, pageSize) +: wikitexts, QL.ParsedTextType)
            } else {
              QL.WikitextValue(instanceEditorForThing(thing, partialContext, params))
            }
          }
          
          case _ => QL.WarningValue("The " + displayName + " method can only be used on Properties, Models and Instances")
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
	        QL.WikitextValue(
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
    PlaceholderTextProp,
    PromptProp,
    InstanceEditPropsProp,
    instanceEditViewProp,
    editMethod,
    editOrElseMethod,
    editAsPicklistMethod,
    editWidthProp,
    FormLineMethod
  )
}