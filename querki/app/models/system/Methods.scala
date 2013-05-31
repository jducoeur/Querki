package models.system

import play.api.Logger
import play.api.templates.Html

import models._
import Property._
import Thing._

import ql._

import OIDs._

import YesNoType._

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
  override def qlApply(context:ContextBase, params:Option[Seq[QLPhrase]] = None):TypedValue = {
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
class SingleThingMethod(tid:OID, name:String, desc:String, action:(Thing, ContextBase) => TypedValue) extends InternalMethod(tid,
    toProps(
      setName(name),
      DisplayTextProp(desc)
    ))
{
  override def qlApply(context:ContextBase, params:Option[Seq[QLPhrase]] = None):TypedValue = {
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
  def handleThing(t:Thing, context:ContextBase):TypedValue = action(t, context)
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
  override def qlApply(context:ContextBase, params:Option[Seq[QLPhrase]] = None):TypedValue = {
    ErrorValue(displayName + " can not be applied on its own; you need to use this on the right-hand side of a dot, as PropertyName." + displayName)
  }
  
  override def partiallyApply(leftContext:ContextBase):QLFunction = {
    def handleRemainder(mainContext:ContextBase, params:Option[Seq[QLPhrase]]):TypedValue = {
      fullyApply(mainContext, leftContext, params)
    }
    new PartiallyAppliedFunction(leftContext, handleRemainder)
  }
  
  /**
   * The actual Method must implement this. It takes both contexts -- the partial context that we were
   * dotted to and the main incoming context -- and does the usual sorts of things with them.
   */
  def fullyApply(mainContext:ContextBase, partialContext:ContextBase, params:Option[Seq[QLPhrase]]):TypedValue
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
      params:Option[Seq[QLPhrase]]):TypedValue
      
  def fullyApply(mainContext:ContextBase, partialContext:ContextBase, params:Option[Seq[QLPhrase]]):TypedValue = {
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
  override def qlApply(context:ContextBase, params:Option[Seq[QLPhrase]] = None):TypedValue = {
    fullyApply(context, context, params)
  }
}

object InstancesMethod extends SingleContextMethod(InstancesMethodOID,
    toProps(
      setName("_instances"),
      DisplayTextProp("Returns all of the non-Model Things that are based on this")))
{
  def fullyApply(mainContext:ContextBase, partialContext:ContextBase, params:Option[Seq[QLPhrase]]):TypedValue = {
    applyToIncomingThing(partialContext)(handleThing)
  }
  
  def handleThing(t:Thing, context:ContextBase):TypedValue = {
    TypedValue(QList.from(context.state.descendants(t.id, false, true).map(_.id), LinkType), LinkType)
  }
}

object EditMethod extends ThingPropMethod(EditMethodOID, 
    toProps(
      setName("_edit"),
      DisplayTextProp("Puts an editor for the specified Property into the page"),
      (AppliesToKindOID -> QList(ElemValue(Kind.Property)))
    )) 
{
  def applyToPropAndThing(mainContext:ContextBase, mainThing:Thing, 
    partialContext:ContextBase, prop:Property[_,_],
    params:Option[Seq[QLPhrase]]):TypedValue =
  {
    val currentValue = mainThing.getDisplayPropVal(prop)(mainContext.state)
    // TODO: conceptually, this is a bit off -- the rendering style shouldn't be hard-coded here. We
    // probably need to have the Context contain the desire to render in HTML, and delegate to the
    // HTML renderer indirectly. In other words, the Context should know the renderer to use, and pass
    // that into here:
    val inputControl = querki.html.HtmlRenderer.renderPropertyInput(mainContext.state, prop, currentValue)
    HtmlValue(inputControl)    
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
  override def qlApply(context:ContextBase, paramsOpt:Option[Seq[QLPhrase]] = None):TypedValue = {
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
  
  def buildSection(context:ContextBase, header:QLPhrase, detailsOpt:Option[QLPhrase], emptyOpt:Option[QLPhrase]):TypedValue = {
    val parser = context.parser.get
    val wikitext = if (context.isEmpty) {
      parser.contextsToWikitext(emptyOpt.map(empty => Seq(parser.processPhrase(empty.ops, context.root))).getOrElse(Seq.empty))
    } else {
      val processedHeader = parser.contextsToWikitext(Seq(parser.processPhrase(header.ops, context.asCollection)))
      val processedDetails = detailsOpt.map(details => parser.processPhrase(details.ops, context))
      processedDetails match {
        // Note that we intentionally always put a newline between the header and details:
        case Some(details) => processedHeader.+(parser.contextsToWikitext(Seq(details)), true)
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
    params:Option[Seq[QLPhrase]]):TypedValue =
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
	  TypedValue(QList.from(results, LinkType), LinkType)
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
  override def qlApply(context:ContextBase, paramsOpt:Option[Seq[QLPhrase]] = None):TypedValue = {
    paramsOpt match {
      case Some(params) => {
        val result = (Option.empty[TypedValue] /: params) { (current, phrase) =>
          current match {
            case Some(result) => Some(result)
            case None => {
              val oneResult = context.parser.get.processPhrase(phrase.ops, context)
              if (oneResult.value.v.isEmpty)
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
        result.getOrElse(EmptyValue(context.value.pt))
      }
      case None => WarningValue("The _or() operator is meaningless if you don't give it any parameters")
    }
  }
}

object NotMethod extends InternalMethod(NotOID,
    toProps(
      setName("_not"),
      DisplayTextProp("_or takes the parameter if one is given, or the received value if not. It returns True iff that it False, and False if it is anything else")))
{
  override def qlApply(context:ContextBase, paramsOpt:Option[Seq[QLPhrase]] = None):TypedValue = {
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
  override def qlApply(context:ContextBase, paramsOpt:Option[Seq[QLPhrase]] = None):TypedValue = {
    val sourceColl = context.value.v
    val result = 
      if (sourceColl.isEmpty)
        Optional.None
      else
        Optional(sourceColl.cv.head)
    TypedValue(result, context.value.pt)
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
  override def qlApply(context:ContextBase, paramsOpt:Option[Seq[QLPhrase]] = None):TypedValue = {
    val sourceColl = context.value.v
    if (sourceColl.isEmpty)
      // Cut processing at this point:
      TypedValue(QList.empty, context.value.pt, true)
    else
      TypedValue(QList.makePropValue(sourceColl.cv.tail.toList), context.value.pt)
  }
}

object LinkButtonMethod extends InternalMethod(LinkButtonOID,
    toProps(
      setName("_linkButton"),
      DisplayTextProp("""_linkButton(LABEL) receives a Link or External Link, and displays that
link as a button. It expects one parameter, which will be the label of the button.
          """)))
{
  override def qlApply(context:ContextBase, paramsOpt:Option[Seq[QLPhrase]] = None):TypedValue = {
    // TODO: This is a horrible hack! How do we get LinkType and ExternalLinkType to give up their URLs
    // in a consistent and type-safe way?
    val url = context.value.pt match {
      case LinkType => LinkType.followLink(context).get.toThingId.toString()
      case ExternalLinkType => ExternalLinkType.get(context.value.v.first).toExternalForm()
    }
    val label = paramsOpt match {
      case Some(Seq(phrase)) => context.parser.get.processPhrase(phrase.ops, context).value.render(context)
      case None => Wikitext("Link")
    }
    HtmlValue(Html("<a class=\"_linkButton\" href=\"" + url + "\">" + label.plaintext + "</a>"))
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
    params:Option[Seq[QLPhrase]]):TypedValue =
  {
    return boolean2YesNoTypedValue(!isEmpty(mainContext, mainThing, prop))
  }
}

object IsEmptyMethod extends ThingPropMethod(IsEmptyOID,
    toProps(
      setName("_isEmpty"),
      DisplayTextProp("THING -> PROP._isEmpty produces true iff PROP is not defined on THING, or this instance contains no elements")))
{
  def applyToPropAndThing(mainContext:ContextBase, mainThing:Thing, 
    partialContext:ContextBase, prop:Property[_,_],
    params:Option[Seq[QLPhrase]]):TypedValue =
  {
    return boolean2YesNoTypedValue(IsNonEmptyMethod.isEmpty(mainContext, mainThing, prop))
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
  override def qlApply(context:ContextBase, paramsOpt:Option[Seq[QLPhrase]] = None):TypedValue = {
    def chooseParam(params:Seq[QLPhrase]):QLPhrase = {
      val received = context.value.v
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

/**
 * TODO: properly speaking, this shouldn't assume QList -- it should work for any Collection!
 */
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
  override def qlApply(context:ContextBase, paramsOpt:Option[Seq[QLPhrase]] = None):TypedValue = {
    
    // TODO: this is currently convoluted and hard to understand -- we're dissecting the list using
    // flatMapAsContext(); yielding an Option saying whether to keep each one; stitching it back together
    // as a Context, and then just using the TypedValue. Bleah.
    def tryElem(parser:QLParser, phrase:QLPhrase)(elem:ContextBase):Option[ElemValue] = {
      val passesYesNo = parser.processPhrase(phrase.ops, elem).value
      for (bool <- passesYesNo.firstTyped(YesNoType) if (bool)) yield elem.value.v.first
     }
    
    val result = for
    (
      params <- paramsOpt if params.length == 1;
      phrase = params(0);
      parser <- context.parser
    )
      yield context.flatMapAsContext(tryElem(parser, phrase), context.value.pt).value
      
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
{ (thing, context) => TypedValue(QList.from(context.state.thingRoots, LinkType), LinkType) })

object SortMethod extends InternalMethod(SortMethodOID,
    toProps(
      setName("_sort"),
      DisplayTextProp("""
    LIST -> _sort -> SORTED
          
With no parameters, _sort sorts the elements of the received List alphabetically by their Display Names.
""")))
{
  override def qlApply(context:ContextBase, paramsOpt:Option[Seq[QLPhrase]] = None):TypedValue = {
    context.value.pt match {
      case LinkType => {
        val start = context.value.v.cv.toSeq
        val asThings = start.map(elemV => context.state.anything(LinkType.get(elemV))).flatten
        val sortedOIDs = asThings.sortWith((left, right) => left.displayName < right.displayName).map(_.id)
        TypedValue(QList.from(sortedOIDs, LinkType), LinkType)
      }
      case _ => WarningValue("_sort can only currently be applied to Links.")
    }
  }
}

object ChildrenMethod extends SingleThingMethod(ChildrenMethodOID, "_children", "This produces the immediate children of the received Model.",
{ (thing, context) => TypedValue(QList.from(context.state.children(thing).map(_.id), LinkType), LinkType) })