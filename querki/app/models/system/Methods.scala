package models.system

import play.api.Logger
import play.api.templates.Html

import models._
import Property._
import Thing._

import ql._

import OIDs._

import YesNoType._

import querki.values._

/**
 * Internal methods -- functions defined in-code that can be assigned as properties -- should
 * inherit from this.
 */
class InternalMethod(tid:OID, p:PropFetcher) extends SystemProperty(tid, InternalMethodType, QUnit, p)
{
  /**
   * Methods should override this to implement their own functionality.
   * 
   * TBD: we probably want to lift out some common patterns, but we'll have to see what
   * those look like.
   */
  override def qlApply(context:ContextBase, params:Option[Seq[QLPhrase]] = None):QValue = {
    // By default, we just pass the incoming context right through:
    context.value
  }
  
  /**
   * Methods are, currently, always QUnit -- that is, they can't have data with them.
   * 
   * TBD: this is actually questionable. What if we allowed them to have their own data with
   * the declaration? That would essentially allow us some simple higher-kinded functions,
   * which might be kinda useful.
   */
  def decl = (tid, QUnit.default(InternalMethodType))
}

/**
 * Convenience class for internal methods that expect to work with a single Thing -- for example,
 * a method that operates on the Thing it is attached to. This is probably going to be the most
 * common type of method.
 * 
 * TBD: action really ought to be a separate parameter list, but for some reason I'm having trouble
 * instantiating it that way. Figure out the syntax and do that.
 */
class SingleThingMethod(tid:OID, name:String, desc:String, action:(Thing, ContextBase) => QValue) extends InternalMethod(tid,
    toProps(
      setName(name),
      DisplayTextProp(desc)
    ))
{
  override def qlApply(context:ContextBase, params:Option[Seq[QLPhrase]] = None):QValue = {
    try {
      applyToIncomingThing(context)(handleThing)
    } catch {
      case error:Exception => Logger.error("Error while running internal method", error)
      ErrorValue("Error while running internal method")
    }
  }
  
  /**
   * Definition of the method needs to define this -- take the incoming Thing (most often, the
   * Thing that the Method is defined upon) and do whatever is appropriate.
   * 
   * Pure side-effecting methods should typically just return the value from the context.
   */
  def handleThing(t:Thing, context:ContextBase):QValue = action(t, context)
}

/**
 * A MetaMethod is a Method that is intended to be dotted -- that is, it should be specified on the
 * right-hand side of a dot. _edit is the canonical example. Usually, these are methods on a Property,
 * which need to be contextualized by the Thing that Property applies to.
 * 
 * TBD: this is a pretty weak version of partial application. At some point, let's see if we can
 * refactor this to be more general, powerful and correct. Also, the interaction of this and
 * PartiallyAppliedFunction is clearly too baroque, and can probably be simplified.
 * 
 * TODO: partial application has been lifted up into Property as a more general concept. This code
 * is still correct, but should probably be refactored into that.
 */
abstract class MetaMethod(tid:OID, p:PropFetcher) extends InternalMethod(tid, p)
{
  override def qlApply(context:ContextBase, params:Option[Seq[QLPhrase]] = None):QValue = {
    ErrorValue(displayName + " can not be applied on its own; you need to use this on the right-hand side of a dot, as PropertyName." + displayName)
  }
  
  override def partiallyApply(leftContext:ContextBase):QLFunction = {
    def handleRemainder(mainContext:ContextBase, params:Option[Seq[QLPhrase]]):QValue = {
      fullyApply(mainContext, leftContext, params)
    }
    new PartiallyAppliedFunction(leftContext, handleRemainder)
  }
  
  /**
   * The actual Method must implement this. It takes both contexts -- the partial context that we were
   * dotted to and the main incoming context -- and does the usual sorts of things with them.
   */
  def fullyApply(mainContext:ContextBase, partialContext:ContextBase, params:Option[Seq[QLPhrase]]):QValue
}

/**
 * This is a specialized but common category of Methods: ones that operate on a specific Property, on a
 * specific Thing. They all expect the syntax "THING -> PROP._method".
 */
abstract class ThingPropMethod(tid:OID, p:PropFetcher) extends MetaMethod(tid, p)
{
  /**
   * Concrete classes should define this method, which is the heart of things.
   */
  def applyToPropAndThing(mainContext:ContextBase, mainThing:Thing, 
      partialContext:ContextBase, prop:Property[_,_],
      params:Option[Seq[QLPhrase]]):QValue
      
  def fullyApply(mainContext:ContextBase, partialContext:ContextBase, params:Option[Seq[QLPhrase]]):QValue = {
    applyToIncomingThing(mainContext) { (mainThing, _) =>
      applyToIncomingThing(partialContext) { (shouldBeProp, _) =>
        shouldBeProp match {
          case prop:Property[_,_] => {
            applyToPropAndThing(mainContext, mainThing, partialContext, prop, params)
          }
          case _ => ErrorValue("The " + displayName + " method can only be used on Properties")
        } 
      }
    }
  }
}

/**
 * This is a syntactically-loose method that you can use in *either* a dotted or normal place,
 * but which really doesn't take any incoming context except for that one. It is intended mainly
 * for beginning-of-phrase methods that intuitively seem like they should be dotted, and which
 * are producing the initial context for the phrase.
 * 
 * TODO: there are some more-consistent abstractions fighting to break out here. I suspect that
 * the division into the various kinds of methods is just plain wrong.
 */
abstract class SingleContextMethod(tid:OID, p:PropFetcher) extends MetaMethod(tid, p)
{
  override def qlApply(context:ContextBase, params:Option[Seq[QLPhrase]] = None):QValue = {
    fullyApply(context, context, params)
  }
}

object InstancesMethod extends SingleContextMethod(InstancesMethodOID,
    toProps(
      setName("_instances"),
      DisplayTextProp("Returns all of the non-Model Things that are based on this")))
{
  def fullyApply(mainContext:ContextBase, partialContext:ContextBase, params:Option[Seq[QLPhrase]]):QValue = {
    applyToIncomingThing(partialContext)(handleThing)
  }
  
  def handleThing(t:Thing, context:ContextBase):QValue = {
    QList.from(context.state.descendants(t.id, false, true).map(_.id), LinkType)
  }
}

abstract class EditMethodBase(id:OID, pf:PropFetcher) extends ThingPropMethod(id, pf)
{
  def cantEditFallback(mainContext:ContextBase, mainThing:Thing, 
    partialContext:ContextBase, prop:Property[_,_],
    params:Option[Seq[QLPhrase]]):QValue
  
  def applyToPropAndThing(mainContext:ContextBase, mainThing:Thing, 
    partialContext:ContextBase, prop:Property[_,_],
    params:Option[Seq[QLPhrase]]):QValue =
  {
    mainContext.request.requester match {
      case Some(requester) if (mainContext.state.canEdit(requester, mainThing.id)) => {
        val currentValue = mainThing.getDisplayPropVal(prop)(mainContext.state)
	    // TODO: conceptually, this is a bit off -- the rendering style shouldn't be hard-coded here. We
	    // probably need to have the Context contain the desire to render in HTML, and delegate to the
	    // HTML renderer indirectly. In other words, the Context should know the renderer to use, and pass
	    // that into here:
	    val inputControl = querki.html.HtmlRenderer.renderPropertyInput(mainContext.state, prop, currentValue)
	    HtmlValue(inputControl)    
      }
      case _ => cantEditFallback(mainContext, mainThing, partialContext, prop, params)
    }
  }
}
object EditMethod extends EditMethodBase(EditMethodOID, 
    toProps(
      setName("_edit"),
      DisplayTextProp("Puts an editor for the specified Property into the page"),
      AppliesToKindProp(Kind.Property)
    )) 
{
  def cantEditFallback(mainContext:ContextBase, mainThing:Thing, 
    partialContext:ContextBase, prop:Property[_,_],
    params:Option[Seq[QLPhrase]]):QValue = {
      // This user isn't allowed to edit, so simply render the property in its default form.
      // For more control, user _editOrElse instead.
      prop.qlApply(mainContext, params)    
  }  
}
object EditOrElseMethod extends EditMethodBase(EditOrElseMethodOID, 
    toProps(
      setName("_editOrElse"),
      DisplayTextProp("PROP._editOrElse(FALLBACK) shows an editor for property PROP if the user is allowed" +
      		"to edit this thing; otherwise, it displays FALLBACK."),
      AppliesToKindProp(Kind.Property)
    )) 
{
  def cantEditFallback(mainContext:ContextBase, mainThing:Thing, 
    partialContext:ContextBase, prop:Property[_,_],
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

object SectionMethod extends InternalMethod(SectionMethodOID,
    toProps(
      setName("_section"),
      DisplayTextProp("""_section is intended for the common case where you want to display a section
on the page if and only if a specific List is non-empty. It looks like this:
    My List -> _section(HEADER, DETAILS, EMPTY)
Each of the parameters can be any QL phrase, although they are usually just text blocks. They are
treated as follows:
          
* HEADER is shown first, if the incoming List is non-empty. It gets the entire List as its Context.
* DETAILS is shown after the header. It is repeated for each element in the List, just as it would
if you fed a List into a normal text block.
* EMPTY is shown if and only if the List is empty. This lets you show something else if appropriate.
It is optional -- you can leave it off.
          """)
    )) 
{
  override def qlApply(context:ContextBase, paramsOpt:Option[Seq[QLPhrase]] = None):QValue = {
    paramsOpt match {
      case Some(params) if (params.length > 0) => {
        val header = params(0)
        val details = if (params.length > 1) Some(params(1)) else None
        val empty = if (params.length > 2) Some(params(2)) else None
        buildSection(context, header, details, empty)
      }
      case _ => ErrorValue("_section requires at least one parameter")
    }
  }  
  
  def buildSection(context:ContextBase, header:QLPhrase, detailsOpt:Option[QLPhrase], emptyOpt:Option[QLPhrase]):QValue = {
    val parser = context.parser.get
    val wikitext = if (context.isEmpty) {
      parser.contextsToWikitext(emptyOpt.map(empty => Seq(parser.processPhrase(empty.ops, context.root))).getOrElse(Seq.empty))
    } else {
      val processedHeader = parser.contextsToWikitext(Seq(parser.processPhrase(header.ops, context.asCollection)))
      // TODO: this pattern is directly adapted from processTextStage. It almost certainly should be refactored out as a
      // higher-level operation of some flavor:
      val processedDetails = detailsOpt.map{details =>
        // We apply the Details Phrase to each element of the incoming context, separately:
        context.map { elemContext =>
          parser.processPhrase(details.ops, elemContext)
        }.toSeq
      }
      // TODO: why are we transforming this to Wikitext this early? Is there any reason to? Shouldn't we just turn all
      // of this into a new List Context and pass it on through? Conceptually that would be more correct. The only problem
      // is that the Header and Details potentially produce different Types, so they might not fit neatly into a single List.
      // Which implies, of course, that what we *should* be producing here is a Tuple of (Header, List[Details]). Hmm --
      // let's revisit this once we have Tuples implemented.
      processedDetails match {
        // Note that we intentionally always put a newline between the header and details, and between each details entry.
        // TODO: this should probably be exposed as an optional parameter in the _section() method.
        case Some(details) => processedHeader.+(parser.contextsToWikitext(details, true), true)
        case None => processedHeader
      }
    }
    WikitextValue(wikitext)
  }
}

object ApplyMethod extends SystemProperty(ApplyMethodOID, QLType, Optional,
    toProps(
      setName("_apply"),
      DisplayTextProp("If you set the _apply property, it will be run when you name this Thing in a QL expression.")))

object RefsMethod extends ThingPropMethod(RefsMethodOID, 
    toProps(
      setName("_refs"),
      DisplayTextProp("""Returns all of the Things that use this Property to point to this Thing.
          
Say that I have Thing A and Thing B, which are both of Model M. Property P is a Link to M.
A.P is pointing to B. In that case, B -> P._refs would return A -- the Thing that is pointing
to B using property P.
          
Note that this returns a List, since any number of Things could be pointing to this.""")))
{
  def applyToPropAndThing(mainContext:ContextBase, mainThing:Thing, 
    partialContext:ContextBase, propErased:Property[_,_],
    params:Option[Seq[QLPhrase]]):QValue =
  {
    if (propErased.pType == LinkType) {
	  // EVIL: is there any decent way to get rid of this? I know that the PType is LinkType, so I know
	  // it's legit; can I tell that to Scala?
	  val prop = propErased.asInstanceOf[Property[OID,_]]
	  val results =
	    for (
	      candidateThing <- mainContext.state.allThings;
	      propAndVal <- candidateThing.getPropOpt(prop)(mainContext.state);
	      if propAndVal.contains(mainThing.id)
	    )
	      yield candidateThing.id;
	  QList.from(results, LinkType)
    } else {
      WarningValue("_refs can only be applied to Links")
    }
  }
}

object OrMethod extends InternalMethod(OrMethodOID,
    toProps(
      setName("_or"),
      DisplayTextProp("""_or() is the short-circuiting "or" operator.
          
_or takes any number of parameters. It runs through each of them, applying the incoming context.
It produces the first one that returns a non-empty result, or None iff all of them come out empty. 
          """)))
{
  override def qlApply(context:ContextBase, paramsOpt:Option[Seq[QLPhrase]] = None):QValue = {
    paramsOpt match {
      case Some(params) => {
        val result = (Option.empty[QValue] /: params) { (current, phrase) =>
          current match {
            case Some(result) => Some(result)
            case None => {
              val oneResult = context.parser.get.processPhrase(phrase.ops, context)
              if (oneResult.value.isEmpty)
                None
              else
                Some(oneResult.value)  
            }
          }
        }
        // If we got nothing out, then produce an empty list of the incoming type
        // TBD: this really isn't the correct type to produce -- ideally, the type should
        // be the one that would be output by the various parameter phrases. How can we
        // suss that?
        result.getOrElse(EmptyValue(context.value.pType))
      }
      case None => WarningValue("The _or() operator is meaningless if you don't give it any parameters")
    }
  }
}

object NotMethod extends InternalMethod(NotOID,
    toProps(
      setName("_not"),
      DisplayTextProp("_not takes the parameter if one is given, or the received value if not. It returns True iff that it False, and False if it is anything else")))
{
  override def qlApply(context:ContextBase, paramsOpt:Option[Seq[QLPhrase]] = None):QValue = {
    val inVal = paramsOpt match {
      case Some(params) if (params.length == 1) => {
        context.parser.get.processPhrase(params(0).ops, context).value
      }
      case _ => context.value
    }
    !YesNoType.toBoolean(inVal)
  }
}

object FirstMethod extends InternalMethod(FirstMethodOID,
    toProps(
      setName("_first"),
      DisplayTextProp("""_first grabs just the first thing from the received context.
          
Often you have a List, and you just want the first item in the List. (Especially when you
expect the list to only have one element in it.) Use _first to turn that List into an
Optional instead.
          """)))
{
  override def qlApply(context:ContextBase, paramsOpt:Option[Seq[QLPhrase]] = None):QValue = {
    val sourceColl = context.value
    val result = 
      if (sourceColl.isEmpty)
        Optional.None
      else
        Optional(sourceColl.cv.head)
    result
  }
}

object RestMethod extends InternalMethod(RestMethodOID,
    toProps(
      setName("_rest"),
      DisplayTextProp("""_rest produces everything but the first thing from the received context.
          
Often you have a List, and you want to slice off the first item (using _first). You then use _rest
to handle everything else.
          """)))
{
  override def qlApply(context:ContextBase, paramsOpt:Option[Seq[QLPhrase]] = None):QValue = {
    val sourceColl = context.value
    if (sourceColl.isEmpty)
      // Cut processing at this point:
      // TODO: can/should we preserve the source PType?
      EmptyListCut()
    else
      QList.makePropValue(sourceColl.cv.tail.toList, context.value.pType)
  }
}

abstract class ButtonBase(tid:OID, pf:PropFetcher) extends InternalMethod(tid, pf)
{
  def generateButton(url:String, params:Seq[Wikitext]):scala.xml.Elem
  
  def numParams:Int
  
  override def qlApply(context:ContextBase, paramsOpt:Option[Seq[QLPhrase]] = None):QValue = {
    paramsOpt match {
      case Some(params) if (params.length == numParams) => {
        // TODO: This is a horrible hack! How do we get LinkType and ExternalLinkType to give up their URLs
        // in a consistent and type-safe way?
        val url = context.value.pType match {
          case LinkType => LinkType.followLink(context).get.toThingId.toString()
          case ExternalLinkType => ExternalLinkType.get(context.value.first).toExternalForm()
        }
        
        val paramTexts = params.map(phrase => context.parser.get.processPhrase(phrase.ops, context).value.render(context))
        HtmlValue(Html(generateButton(url, paramTexts).toString))        
      }
      case None => WarningValue(displayName + " requires " + numParams + " parameters.")
    }
  }
}

object LinkButtonMethod extends ButtonBase(LinkButtonOID,
    toProps(
      setName("_linkButton"),
      DisplayTextProp("""_linkButton(LABEL) receives a Link or External Link, and displays that
link as a button. It expects one parameter, which will be the label of the button.
          """)))
{
  val numParams = 1
  
  def generateButton(url:String, params:Seq[Wikitext]):scala.xml.Elem = {
    <a class="btn btn-primary" href={url}>{params(0).raw}</a>
  }
}

object IconButtonMethod extends ButtonBase(IconButtonOID,
    toProps(
      setName("_iconButton"),
      DisplayTextProp("""_iconButton(ICON, TOOLTIP) receives a Link or External Link, and displays that
link as a button. The first parameter identifies the icon to use for the button; the second is the
hover text to display as a tooltip.""")))
{
  val numParams = 2
  
  def generateButton(url:String, params:Seq[Wikitext]):scala.xml.Elem = {
    <a class="btn btn-mini btn-primary" href={url} title={params(1).raw}><i class={params(0).raw + " icon-white"}></i></a>
  }
}

/**
 * TBD: is this the correct definition of _isEmpty and _isNonEmpty? It feels slightly off to me, to have it specifically depend
 * on the instance like this. But otherwise, we have problems because a property is essentially *always* non-Empty if it if defined,
 * due to defaults.
 * 
 * Maybe the correct solution is a little more nuanced, that a property is considered "empty" if its value is the default?
 */
object IsNonEmptyMethod extends ThingPropMethod(IsNonEmptyOID,
    toProps(
      setName("_isNonEmpty"),
      DisplayTextProp("THING -> PROP._isNonEmpty produces true iff PROP is defined on THING, and this instance contains at least one element")))
{
  override def qlApply(context:ContextBase, params:Option[Seq[QLPhrase]] = None):QValue = {
    boolean2YesNoQValue(!context.value.isEmpty)
  }

  def isEmpty(mainContext:ContextBase, mainThing:Thing, prop:Property[_,_]):Boolean = {
    implicit val s = mainContext.state
    val isEmpty = for (
      propAndVal <- mainThing.localProp(prop)
    )
      yield propAndVal.isEmpty
    
    isEmpty.getOrElse(true)
  }
  
  def applyToPropAndThing(mainContext:ContextBase, mainThing:Thing, 
    partialContext:ContextBase, prop:Property[_,_],
    params:Option[Seq[QLPhrase]]):QValue =
  {
    return boolean2YesNoQValue(!isEmpty(mainContext, mainThing, prop))
  }
}

object IsEmptyMethod extends ThingPropMethod(IsEmptyOID,
    toProps(
      setName("_isEmpty"),
      DisplayTextProp("THING -> PROP._isEmpty produces true iff PROP is not defined on THING, or this instance contains no elements")))
{
  override def qlApply(context:ContextBase, params:Option[Seq[QLPhrase]] = None):QValue = {
    boolean2YesNoQValue(context.value.isEmpty)
  }

  def applyToPropAndThing(mainContext:ContextBase, mainThing:Thing, 
    partialContext:ContextBase, prop:Property[_,_],
    params:Option[Seq[QLPhrase]]):QValue =
  {
    return boolean2YesNoQValue(IsNonEmptyMethod.isEmpty(mainContext, mainThing, prop))
  }
}

object PluralizeMethod extends InternalMethod(PluralizeOID,
    toProps(
      setName("_pluralize"),
      DisplayTextProp("""
    RECEIVED -> _pluralize(SINGULAR,PLURAL)
          
Is a convenient method for choosing different text depending on a Property. The RECEIVED
Context should usually be a List. If it contains a single element, _pluralize produces
SINGULAR; if it contains multiple *or* zero elements, _pluralize produces PLURAL.
         
Note that this behaviour is pretty English-specific. We expect that other variations will
be needed for other languages in the long run.
          """)))
{
  override def qlApply(context:ContextBase, paramsOpt:Option[Seq[QLPhrase]] = None):QValue = {
    def chooseParam(params:Seq[QLPhrase]):QLPhrase = {
      val received = context.value
      if (received.isEmpty || received.size > 1)
        params(1)
      else
        params(0)
    }
    
    val result = for
    (
      params <- paramsOpt if params.length == 2;
      phrase = chooseParam(params);
      parser <- context.parser
    )
      yield parser.processPhrase(phrase.ops, context.asCollection).value
      
    result.getOrElse(WarningValue("_pluralize requires exactly two parameters"))
  }
}

object FilterMethod extends InternalMethod(FilterOID,
    toProps(
      setName("_filter"),
      DisplayTextProp("""
    RECEIVED -> _filter(FILTER)
          
This function is how you take a List of things, and whittle them down to just the ones you want.
          
The FILTER should take a Thing, and produce a YesNo that says whether to include this Thing.
That gets applied to each element of RECEIVED; if FILTER returns Yes, then it is included, otherwise not.
          """)))
{
  override def qlApply(context:ContextBase, paramsOpt:Option[Seq[QLPhrase]] = None):QValue = {
    
    // TODO: this is currently convoluted and hard to understand -- we're dissecting the list using
    // flatMapAsContext(); yielding an Option saying whether to keep each one; stitching it back together
    // as a Context, and then just using the QValue. Bleah.
    def tryElem(parser:QLParser, phrase:QLPhrase)(elem:ContextBase):Option[ElemValue] = {
      val passesYesNo = parser.processPhrase(phrase.ops, elem).value
      // TODO: I think this crashes if passesYesNo doesn't return a YesNo! It should
      // fail more gracefully. This is a good illustration of why firstTyped is evil.
      for (bool <- passesYesNo.firstAs(YesNoType) if (bool)) yield elem.value.first
    }
    
    val result = for
    (
      params <- paramsOpt if params.length == 1;
      phrase = params(0);
      parser <- context.parser
    )
      yield context.flatMapAsContext(tryElem(parser, phrase), context.value.pType).value
      
    result.getOrElse(WarningValue("_filter requires exactly one parameter"))
  }
}

object SpaceMethod extends SingleThingMethod(SpaceMethodOID, "_space", """
    RECEIVED -> _space -> SPACE
          
This function produces the Space that the received Thing is contained in.
          """,
{ (thing, context) => LinkValue(thing.spaceId) })

object ExternalRootsMethod extends SingleThingMethod(ExternalRootsOID, "_externalRoots", """
    SPACE -> _externalRoots -> ROOTS
   
Pass in a link to a Space; this produces all of the "roots" -- the Things from its Apps -- used
by that Space.""",
{ (thing, context) => QList.from(context.state.thingRoots, LinkType) })

object AllPropsMethod extends SingleThingMethod(AllPropsMethodOID, "_allProps", """
    SPACE -> _allProps -> PROPS
    
This receives a link to a Space, and produces all of the Properties defined in that Space.""",
{ (thing, context) => 
  thing match {
    case s:SpaceState => QList.from(s.propList.toSeq.sortBy(_.displayName), LinkFromThingBuilder) 
    case _ => WarningValue("_allProps must be used with a Space")
  }
  
})

object SortMethod extends InternalMethod(SortMethodOID,
    toProps(
      setName("_sort"),
      DisplayTextProp("""
    LIST -> _sort(EXP) -> SORTED
          
With no parameters, _sort sorts the elements of the received List alphabetically by their Display Names.
If a parameter is given, it is applied to each element in LIST, and the results are used to sort the
elements. This may produce strange results if EXP is not defined on all the elements!
""")))
{
  override def qlApply(context:ContextBase, paramsOpt:Option[Seq[QLPhrase]] = None):QValue = {
    
    implicit val s = context.state
    implicit val rc = context.request

    // TODO: this is awfully inefficient -- we're recomputing the processPhrase repeatedly during
    // the sort process. We should probably instead map the parameter over the list, and then
    // sort using the results:
    def thingSortFunc(left:Thing, right:Thing):Boolean = {
      val sortResult =
        for (
            params <- paramsOpt;
            leftResult = context.parser.get.processPhrase(params(0).ops, context.next(ExactlyOne(LinkType(left)))).value;
            rightResult = context.parser.get.processPhrase(params(0).ops, context.next(ExactlyOne(LinkType(right)))).value;
            // Note that we have to compare their *classes*, so that _desc -- which produces a separate pseudo-Type each time
            // -- can work:
            if (leftResult.pType.getClass() == rightResult.pType.getClass());
            // If the two values are equal, fall through to the default:
            if (!leftResult.pType.matches(leftResult.first, rightResult.first))
          )
          yield leftResult.pType.comp(context)(leftResult.first, rightResult.first)
          
      // Default to sorting by displayName if anything doesn't work correctly:
      sortResult.getOrElse(left.displayName < right.displayName)
    }
    
    context.value.pType match {
      case LinkType => {
        val start = context.value.cv.toSeq
        // TODO: we probably don't need to translate these to Things any more:
        val asThings = start.map(elemV => context.state.anything(LinkType.get(elemV))).flatten
        val sortedOIDs = asThings.sortWith(thingSortFunc).map(_.id)
        // TODO: there is obviously a refactoring screaming to break free here, but it involves some fancy
        // type math. How do we lift things so that we can do QList.from() an arbitrary PType? (Remember that
        // it expects a PTypeBuilder, *and* requires that the input Iterable be of the expected RT.)
        QList.from(sortedOIDs, LinkType)
      }
      case TagSetType => {
        val names = context.value.rawList(TagSetType)
        val sorted = names.sorted
        QList.from(sorted, TagSetType)
      }
      case _ => WarningValue("_sort can only currently be applied to Links.")
    }
  }
}

/**
 * A pseudo-Type, which exists solely for the _desc method. This is a DelegatingType that is exactly like the one
 * it wraps around, except that it has a reversed sort order.
 */
class DescendingType[VT](baseType: PType[VT]) extends DelegatingType[VT](baseType) {
  override def doComp(context:ContextBase)(left:VT, right:VT):Boolean = !realType.doComp(context)(left, right)
}

object DescMethod extends InternalMethod(DescMethodOID,
    toProps(
      setName("_desc"),
      DisplayTextProp("""LIST -> _sort(_desc(EXP)) -> SORTED
          |
          |_desc returns the given EXP, tweaked so that the values in it have the reversed sort order from
          |what they would normally have. It is usually used inside of _sort, to reverse the sort order.""".stripMargin)))
{
  override def qlApply(context:ContextBase, paramsOpt:Option[Seq[QLPhrase]] = None):QValue = {
    paramsOpt match {
      case Some(params) => {
        val innerRes = context.parser.get.processPhrase(params(0).ops, context).value;
        innerRes.cType.makePropValue(innerRes.cv, new DescendingType(innerRes.pType))
      }
      case None => WarningValue("_desc is meaningless without a parameter")
    }
  }
}

object ChildrenMethod extends SingleThingMethod(ChildrenMethodOID, "_children", "This produces the immediate children of the received Model.",
{ (thing, context) => QList.from(context.state.children(thing).map(_.id), LinkType) })

object IsModelMethod extends SingleThingMethod(IsModelMethodOID, "_isModel", "This produces Yes if the received Thing is a Model.",
{ (thing, context) => ExactlyOne(thing.isModel(context.state)) })

// TODO: this is so full of abstraction breaks it isn't funny. Using routes here is inappropriate; indeed, the fact that we're referring
// to Play at all in this level is inappropriate. This probably needs to be routed through the rendering system, so that it takes the
// current rendering environment and produces a relative control appropriate within it. But it'll do for the short term.
import controllers.routes
object CreateInstanceLinkMethod extends SingleThingMethod(CreateInstanceLinkOID, "_createInstanceLink", "Given a received Model, this produces a Link to create an instance of that Model.",
{ (thing, context) => 
  implicit val req = context.request.request
  ExactlyOne(
    ExternalLinkType(routes.Application.createThing(context.request.ownerId.toThingId, context.state.toThingId, Some(thing.toThingId)).absoluteURL()))
})

// TODO: this will become clearer and easier to use once we introduce block-syntax parameters.
object IfMethod extends InternalMethod(IfMethodOID,
    toProps(
      setName("_if"),
      DisplayTextProp("""
    RECEIVED -> _if(YESNO, IFCLAUSE, ELSECLAUSE) -> ...

_if is one of the basic building blocks of programming. It applies the YESNO phrase to the received context.
If the result is Yes, it applies the IFCLAUSE to the received context and produces that. Otherwise, if there
is an ELSECLAUSE, it applies and produces that, or produces nothing if there is no ELSECLAUSE.
          """)))
{
  override def qlApply(context:ContextBase, paramsOpt:Option[Seq[QLPhrase]] = None):QValue = {
    paramsOpt match {
      case Some(params) if (params.length > 1) => {
        val predicatePhrase = params(0)
        val ifCase = params(1)
        val predResult = context.parser.get.processPhrase(predicatePhrase.ops, context)
        if (YesNoType.toBoolean(predResult.value)) {
          context.parser.get.processPhrase(ifCase.ops, context).value
        } else if (params.length > 2) {
          val elseCase = params(2)
          context.parser.get.processPhrase(elseCase.ops, context).value
        } else {
          // TODO: the type here is chosen arbitrarily, but it *should* be the same type as the ifCase.
          EmptyValue(YesNoType)
        }
      }
      case _ => WarningValue("_if requires at least two parameters.")
    }
  }
}

object JoinMethod extends InternalMethod(JoinMethodOID,
    toProps(
      setName("_join"),
      DisplayTextProp("""LIST -> _join(OPEN, SEP, CLOSE]) -> WIKITEXT

_join takes the given LIST, and turns it into a single line. If there is only one parameter, it is SEP,
which is placed between each pair of elements. If there are two, the first is OPEN, which is put at the
beginning. If there are three, the last is CLOSE, which is put at the end.""")))
{
  override def qlApply(context:ContextBase, paramsOpt:Option[Seq[QLPhrase]] = None):QValue = {
    val (openPhrase, sepPhrase, closePhrase) = paramsOpt match {
      case Some(params) if (params.length == 1) => (None, Some(params(0)), None)
      case Some(params) if (params.length == 2) => (Some(params(0)), Some(params(1)), None)
      case Some(params) if (params.length > 2) => (Some(params(0)), Some(params(1)), Some(params(2)))
      case _ => (None, None, None)
    }
    def renderParam(paramOpt:Option[QLPhrase]):Wikitext = {
      paramOpt match {
        case Some(param) => {
          val collContext = context.asCollection
          val paramVal = context.parser.get.processPhrase(param.ops, collContext).value
          val renderedParam = paramVal.pType.render(context)(paramVal.first)
          renderedParam
        }
        case _ => Wikitext.empty
      }
    }

    val elemT = context.value.pType
    val renderedList = context.value.cv.map{elem => elemT.render(context)(elem)}
    val result =
      if (renderedList.isEmpty) {
        Wikitext.empty
      } else {
        val sep = renderParam(sepPhrase)
        renderParam(openPhrase) + (renderedList.head /: renderedList.tail) ((total, next) => total + sep + next) + renderParam(closePhrase)
      }
    WikitextValue(result)
  }
}

object TagRefsMethod extends InternalMethod(TagRefsOID,
    toProps(
      setName("_tagRefs"),
      DisplayTextProp("This produces a List of all Things that have the received Thing or Name as a Tag")))
{ 
  override def qlApply(context:ContextBase, paramsOpt:Option[Seq[QLPhrase]] = None):QValue = {
    val elemT = context.value.pType
    elemT match {
      case nameable:NameableType => {
        val allProps = context.state.allProps.values
        val tagProps = allProps.filter(_.pType == TagSetType)
        val name = nameable.getName(context)(context.value.first)
        val candidates = context.state.allThings
        
        def hasThisTag(candidate:Thing):Boolean = {
          tagProps.exists{ prop =>
            val propAndVal = candidate.localProp(prop)
            val candidateTags:Option[List[String]] = propAndVal.map(_.v.rawList(TagSetType))
            val found = candidateTags.map(_.exists { candidateName =>
              NameType.canonicalize(candidateName) == name
            })
            found.getOrElse(false)
          }
        }
        
        QList.from(candidates.filter(hasThisTag), LinkFromThingBuilder)
      }
      case _ => WarningValue("_tagRefs can only be used with a Tag or Link")
    }
  }
}

object TagsForPropertyMethod extends SingleContextMethod(TagsForPropertyOID,
    toProps(
      setName("_tagsForProperty"),
      DisplayTextProp("""My Tags Property._tagsForProperty -> ... all tags...
          
_tagsForProperty can be used on any Property whose Type is Tag Set. It produces a list of all of the
tags that have been used in that Property so far.
          """)))
{
  def fetchTags(space:SpaceState, propIn:Property[_,_]):Set[String] = {
    implicit val s = space
    val prop = propIn.confirmType(TagSetType)
    val thingsWithProp = space.thingsWithProp(prop)
    (Set.empty[String] /: thingsWithProp) { (set, thing) =>
      set ++ thing.getProp(prop).rawList
    }
  }
  
  def fullyApply(mainContext:ContextBase, partialContext:ContextBase, params:Option[Seq[QLPhrase]]):QValue = {
    applyToIncomingThing(partialContext) { (shouldBeProp, _) =>
      shouldBeProp match {
        case prop:Property[_,_] if (prop.pType == TagSetType) => {
          QList.from(fetchTags(partialContext.state, prop), TagSetType)
        }
        case _ => WarningValue("The _tagsForProperty method can only be used on Tag Set Properties")
      } 
    }    
  }
}

object SelfMethod extends SingleContextMethod(SelfMethodOID,
    toProps(
      setName("_self"),
      DisplayTextProp("""*thing*._self simply produces *thing*.
          
This seems silly, but it is useful for overriding the usual _apply behavior. In particular,
*property*._self is the way to get a link to the property itself, instead of fetching the value
of the property on the received Thing.""")))
{
  def fullyApply(mainContext:ContextBase, partialContext:ContextBase, params:Option[Seq[QLPhrase]]):QValue = {
    partialContext.value
  }
}

object PropsOfTypeMethod extends SingleThingMethod(PropsOfTypeOID, "_propsOfType", "This receives a Type, and produces all of the Properties in this Space with that Type",
{ (thing, context) =>
  thing match {
    case pt:PType[_] => QList.from(context.state.propsOfType(pt), LinkFromThingBuilder)
    case _ => WarningValue("_propsOfType can only be used on a Type")
  }
})

object CodeMethod extends SingleContextMethod(CodeMethodOID,
    toProps(
      setName("_code"),
      DisplayTextProp("""_code() displays the raw code of a value or property, pretty flexibly.
          
You can give it as "TEXT -> _code" to display the TEXT -- however, note that the TEXT will be processed as normal
in this case. If you want to show some raw code, unprocessed, do it as "_code(TEXT)" instead.
          
You can give a property as a parameter -- "_code(PROP)" -- and it will display the value of the property on this Thing.
          
Or you can give a property on some other Thing -- "_code(THING.PROP)" -- to display the value of the property on that Thing.
          
If you have a parameter, and it doesn't work as either PROP or THING.PROP, then it will display the parameter literally.
          """)))
{
  def encodeString(str:String):QValue = {
    val escaped = scala.xml.Utility.escape(str)
    HtmlValue(Html("<pre>" + escaped + "</pre>"))    
  }
  
  def encode(propVal:QValue, pType:PType[_]):QValue = {
    if (propVal.isEmpty)
      WarningValue("_code got an empty input")
    else {
      pType match {
        case codeType:CodeType => {
          val str = codeType.code(propVal.first)
          encodeString(str)
        }
        case ParsedTextType => {
          encodeString(propVal.firstTyped(ParsedTextType).map(_.plaintext).getOrElse(""))
        }
        case _ => WarningValue("_code doesn't work with type " + pType.displayName)
      }
    }
  }
  
  def encodeThingAndProp(thing:Thing, prop:Thing)(implicit space:SpaceState):Option[QValue] = {
    val propAndValOpt = thing.getPropOpt(prop.id)
    propAndValOpt.map { propAndVal => 
      encode(propAndVal.v, propAndVal.prop.pType)
    }
  }
  
  // TODO: this is horrible. Surely we can turn this into something cleaner with better use of the functional
  // tools in the Scala toolbelt.
  def fullyApply(mainContext:ContextBase, partialContext:ContextBase, paramsOpt:Option[Seq[QLPhrase]]):QValue = {
    implicit val space = partialContext.state
    paramsOpt match {
      case Some(params) => {
        // TODO: the way we're handling this is horrible and hard-coded, and needs re-examination. The thing is,
        // we *mostly* don't want to process this. Specifically, we don't want to process the last step of this.
        // For the moment, we're hard-codedly checking the first stage of the phrase and using that, but it should
        // probably process everything until the last stage, and return that stage.
        val phrase = params.head
        val stage = phrase.ops.head
        stage match {
          case QLTextStage(contents, _) => encodeString(contents.reconstructString)
          case QLCall(name, methodNameOpt, _, _) => {
            methodNameOpt match {
              case Some(methodName) => {
                val resultOpt = for (
                  thing <- space.anythingByName(name);
                  propThing <- space.anythingByName(methodName);
                  encoded <- encodeThingAndProp(thing, propThing)
                )
                  yield encoded
                  
                resultOpt.getOrElse(encodeString(phrase.reconstructString))
              }
              case None => {
                val propOpt = space.anythingByName(name)
                propOpt match {
                  case Some(propThing) => {
                    applyToIncomingThing(mainContext) { (thing, _) =>
                      encodeThingAndProp(thing, propThing).getOrElse(encodeString(phrase.reconstructString))
                    }
                  }
                  case None => encodeString(phrase.reconstructString)
                }
              }
            }
          }
        }
      }
      case None => {
        encode(partialContext.value, partialContext.value.pType)
      }
    }
  }
}

object IsDefinedMethod extends SingleContextMethod(IsDefinedOID,
    toProps(
      setName("_isDefined"),
      DisplayTextProp("_isDefined produces Yes if the name passed into it is a real Thing")))
{
  def fullyApply(mainContext:ContextBase, partialContext:ContextBase, paramsOpt:Option[Seq[QLPhrase]]):QValue = {
    partialContext.value.pType != UnknownNameType
  }
}

object CountMethod extends SingleContextMethod(CountMethodOID,
    toProps(
      setName("_count"),
      DisplayTextProp("_count produces the number of elements in the received Collection")))
{
  def fullyApply(mainContext:ContextBase, partialContext:ContextBase, paramsOpt:Option[Seq[QLPhrase]]):QValue = {
    ExactlyOne(IntType(partialContext.value.cv.size))
  }
}

// TODO: this code is pretty damned Bootstrap-specific, which by definition is too HTML-specific. We should probably
// replace it with something that is much more neutral -- simple label/control styles -- and have client-side code
// that rewrites it appropriately for the UI in use.
object FormLineMethod extends SingleContextMethod(FormLineMethodOID,
    toProps(
      setName("_formLine"),
      DisplayTextProp("_formLine(LABEL,CONTROL) displays the LABEL/CONTROL pair as a standard full-width line. This is mainly for input forms.")))
{
  def fullyApply(mainContext:ContextBase, partialContext:ContextBase, paramsOpt:Option[Seq[QLPhrase]]):QValue = {
    paramsOpt match {
      case Some(params) if (params.length == 2) => {
        val context = partialContext
        val label = context.parser.get.processPhrase(params(0).ops, context).value
        val control = context.parser.get.processPhrase(params(1).ops, context).value
        WikitextValue(
          Wikitext("\n{{form-horizontal:\n{{control-group:\n{{control-label:\n") +
          label.render(context) +
          Wikitext("\n}}\n{{controls:\n") +
          control.render(context) +
          Wikitext("\n}}\n}}\n}}\n"))
      }
      case _ => WarningValue("_formLine requires two parameters")
    }
  }
}

object ReverseMethod extends SingleContextMethod(ReverseMethodOID,
    toProps(
      setName("_reverse"),
      DisplayTextProp("_reverse produces the same Collection it receives, as a List, in reverse order")))
{
  def fullyApply(mainContext:ContextBase, partialContext:ContextBase, paramsOpt:Option[Seq[QLPhrase]]):QValue = {
    QList.makePropValue(partialContext.value.cv.toSeq.reverse.toList, partialContext.value.pType)
  }
}

object OIDMethod extends SingleThingMethod(OIDMethodOID, "_oid", """THING -> _oid -> Text
    |
    |This function produces the unique Object ID (which will generally be a period followed by some letters and numbers)
    |of the received Thing.""".stripMargin,
{ (thing, context) => TextValue(thing.id.toThingId) })

object KindMethod extends SingleThingMethod(KindMethodOID, "_kind", """THING -> _kind -> Number
    |
    |This function produces the Number that represents the "kind"
    |of the received Thing. The Kinds are:
    |
    |* Normal Thing: 0
    |* Type: 1
    |* Property: 2
    |* Space: 3
    |* Collection: 4
    |* Attachment: 5""".stripMargin,
{ (thing, context) => ExactlyOne(IntType(thing.kind)) })

object CurrentSpaceMethod extends SingleThingMethod(CurrentSpaceMethodOID, "_currentSpace", """THING -> _currentSpace -> SPACE
    |
    |This function produces the Space that we are currently displaying. (Generally, the one in the URL.)""".stripMargin,
{ (thing, context) => LinkValue(context.root.state) })

object IsMethod extends InternalMethod(IsMethodOID,
    toProps(
      setName("_is"),
      DisplayTextProp("""    THING -> _is(THING) -> Yes or No
    |
    |This function produces Yes iff the parameter matches the passed-in THING, and No otherwise. It is almost always used
    |inside _if(). For instance, to check whether a Property is of Text Type:
    |    MyProp.Property Type -> _if(_is(Text Type), ...)""".stripMargin)))
{ 
  override def qlApply(context:ContextBase, paramsOpt:Option[Seq[QLPhrase]] = None):QValue = {
    paramsOpt match {
      case Some(params) => {
        val contextValOpt = context.value.firstAs(LinkType)
        contextValOpt match {
          case Some(contextOid) => {
            val paramValOpt = context.parser.get.processPhrase(params(0).ops, context).value.firstAs(LinkType)
            paramValOpt match {
              case Some(paramOid) => {
                ExactlyOne(contextOid == paramOid)
              }
              case _ => WarningValue("Parameter of _is didn't produce a Thing")
            }
          }
          case None => WarningValue("_is didn't receive a Thing value")
        }
      }
      case None => WarningValue("_is is meaningless without a parameter")
    }
  }
}