package models.system

import play.api.Logger
import play.api.templates.Html

import models._
import Property._
import Thing._

import ql._

import OIDs._

import YesNoType._

import querki.util._
import querki.values._

object IsFunctionProp extends SystemProperty(IsFunctionOID, YesNoType, ExactlyOne,
    toProps(
      setName("Is Function"),
      SkillLevel(SkillLevel.Advanced),
      Summary("True iff this Thing is a Function."),
      Details("""This is a marker flag that you can put on a Thing to say that it is a Function.
          |This doesn't particularly change the way the Thing works, but has some UI effects.""".stripMargin)))

/**
 * Internal methods -- functions defined in-code that can be assigned as properties -- should
 * inherit from this.
 */
class InternalMethod(tid:OID, p:PropFetcher) extends SystemProperty(tid, InternalMethodType, QUnit, () => (p() + IsFunctionProp(true)))
{
  /**
   * Methods should override this to implement their own functionality.
   * 
   * TBD: we probably want to lift out some common patterns, but we'll have to see what
   * those look like.
   */
  override def qlApply(context:QLContext, params:Option[Seq[QLPhrase]] = None):QValue = {
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
class SingleThingMethod(tid:OID, name:String, summary:String, details:String, action:(Thing, QLContext) => QValue) extends InternalMethod(tid,
    toProps(
      setName(name),
      Summary(summary),
      Details(details)
    ))
{
  override def qlApply(context:QLContext, params:Option[Seq[QLPhrase]] = None):QValue = {
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
  def handleThing(t:Thing, context:QLContext):QValue = action(t, context)
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
  override def qlApply(context:QLContext, params:Option[Seq[QLPhrase]] = None):QValue = {
    ErrorValue(displayName + " can not be applied on its own; you need to use this on the right-hand side of a dot, as PropertyName." + displayName)
  }
  
  override def partiallyApply(leftContext:QLContext):QLFunction = {
    def handleRemainder(mainContext:QLContext, params:Option[Seq[QLPhrase]]):QValue = {
      fullyApply(mainContext, leftContext, params)
    }
    new PartiallyAppliedFunction(leftContext, handleRemainder)
  }
  
  /**
   * The actual Method must implement this. It takes both contexts -- the partial context that we were
   * dotted to and the main incoming context -- and does the usual sorts of things with them.
   */
  def fullyApply(mainContext:QLContext, partialContext:QLContext, params:Option[Seq[QLPhrase]]):QValue
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
  def applyToPropAndThing(mainContext:QLContext, mainThing:Thing, 
      partialContext:QLContext, prop:Property[_,_],
      params:Option[Seq[QLPhrase]]):QValue
      
  def fullyApply(mainContext:QLContext, partialContext:QLContext, params:Option[Seq[QLPhrase]]):QValue = {
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
  override def qlApply(context:QLContext, params:Option[Seq[QLPhrase]] = None):QValue = {
    fullyApply(context, context, params)
  }
}

object InstancesMethod extends SingleContextMethod(InstancesMethodOID,
    toProps(
      setName("_instances"),
      Summary("Returns all of the non-Model Things that are based on this"),
      Details("""A Model is sort of like the concept of a Thing: "Person" or "CD" or "Recipe".
          |
          |An Instance is an actual Thing based on one of those Models: "Joe" or "In Through the Out Door" or "Macaroni and Cheese".
          |
          |Most of the time, when using Querki, you want to create one or more Models that describe the *kinds*
          |of Things you're interested in, and then create a bunch of Instances of each Model.
          |
          |So _instances looks like this:
          |    MODEL -> _instances -> LIST OF INSTANCES
          |That is, it receives a *Model*, and produces the Instances that come from that Model.
          |
          |If you have sub-Models under *Model* (that add more Properties, for example), this will include those as well.""".stripMargin)))
{
  def fullyApply(mainContext:QLContext, partialContext:QLContext, params:Option[Seq[QLPhrase]]):QValue = {
    applyToIncomingThing(partialContext)(handleThing)
  }
  
  def handleThing(t:Thing, context:QLContext):QValue = {
    QList.from(context.state.descendants(t.id, false, true).map(_.id), LinkType)
  }
}

object SectionMethod extends InternalMethod(SectionMethodOID,
    toProps(
      setName("_section"),
      Summary("Display a List as a Header, followed by its contents"),
      Details("""_section is intended for the common case where you want to display a section
          |on the page if and only if a specific List is non-empty. It looks like this:
          |    My List -> _section(HEADER, DETAILS, EMPTY)
          |Each of the parameters can be any QL phrase, although they are often just text blocks. They are
          |treated as follows:
          |
          |* HEADER is shown first, if the incoming List is non-empty. It gets the entire List as its Context.
          |* DETAILS is shown after the header. It is repeated for each element in the List, just as it would
          |if you fed a List into a normal text block.
          |* EMPTY is shown if and only if the List is empty. This lets you show something else if appropriate.
          |It is optional -- you can leave it off.
          |
          |Note that the generated QText will have the HEADER on a separate line from the DETAILS. This is
          |usually what you want. It means that, for example, if you start the HEADER with "###", it will show
          |up as a true header, separate from the DETAILS, but if it is just something like "Contents:", the
          |HEADER and DETAILS will run together, since QText joins ordinary text lines together.""".stripMargin)
    )) 
{
  override def qlApply(context:QLContext, paramsOpt:Option[Seq[QLPhrase]] = None):QValue = {
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
  
  def buildSection(context:QLContext, header:QLPhrase, detailsOpt:Option[QLPhrase], emptyOpt:Option[QLPhrase]):QValue = {
    val parser = context.parser.get
    val wikitext = if (context.isEmpty) {
      parser.contextsToWikitext(emptyOpt.map(empty => Seq(parser.processPhrase(empty.ops, context.root))).getOrElse(Seq.empty))
    } else {
      val processedHeader = parser.contextsToWikitext(Seq(parser.processPhrase(header.ops, context.asCollection)))
      val processedDetails = detailsOpt.map(details => Seq(parser.processPhrase(details.ops, context)))
      // TODO: why are we transforming this to Wikitext this early? Is there any reason to? Shouldn't we just turn all
      // of this into a new List Context and pass it on through? Conceptually that would be more correct. The only problem
      // is that the Header and Details potentially produce different Types, so they might not fit neatly into a single List.
      // Which implies, of course, that what we *should* be producing here is a Tuple of (Header, List[Details]). Hmm --
      // let's revisit this once we have Tuples implemented.
      processedDetails match {
        // Note that there is automatically a newline inserted between the Header and Details. Most of the time, this
        // produces exactly the right result:
        case Some(details) => processedHeader + parser.contextsToWikitext(details, true)
        case None => processedHeader
      }
    }
    WikitextValue(wikitext)
  }
}

object RefsMethod extends ThingPropMethod(RefsMethodOID, 
    toProps(
      setName("_refs"),
      Summary("""Returns all of the Things that use this Property to point to this Thing."""),
      Details("""    THING -> PROPERTY._refs -> REFERRING THINGS
          |Say that my Space is listing my CD collection. I have a Model *Album* for individual discs,
          |and Model *Artist* for performers. Album has a Property *Artists*, which is a Set of Links
          |to Artist -- basically, the list of performers on this particular CD.
          |
          |In this case, *Artist* is likely to want to say something like:
          |[[_code(""[[Artists._refs -> _bulleted]]"")]]
          |That is, based on the Artist we're looking at (which is always the initial Context passed into
          |a QL Expression), get all the Things that refer to this Artist using the *Artists* Property,
          |and display them as a bullet list.
          |
          |This method is enormously useful -- most Models that get pointed to like this will probably
          |want to use it.
          |
          |Note that this always returns a List, since any number of Things could be pointing to this.""".stripMargin)))
{
  def applyToPropAndThing(mainContext:QLContext, mainThing:Thing, 
    partialContext:QLContext, propErased:Property[_,_],
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
      Summary("""The short-circuiting "or" operator."""),
      Details("""    RECEIVED -> _or(CLAUSE1, CLAUSE2, ...) -> RESULT
          |_or takes any number of parameters. It runs through each of them, applying the incoming context.
          |It produces the first one that returns a non-empty result, or None iff all of them come out empty. 
          |
          |IMPORTANT: this method is going to change in the future! We will likely enhance it so that it does
          |the obvious thing if the clauses return a single True/False result. (Or if there is a single parameter,
          |or'ing together the results from that.)""".stripMargin)))
{
  override def qlApply(context:QLContext, paramsOpt:Option[Seq[QLPhrase]] = None):QValue = {
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
      Summary("Returns the reverse of the received value, or the parameter"),
      Details("""    TRUE/FALSE -> _not -> FALSE/TRUE
          |or
          |    RECEIVED -> _not(TRUE/FALSE) -> FALSE/TRUE
          |
          |_not takes the parameter if one is given, or the received value if not. It returns True iff that it False, and False if it is anything else""".stripMargin)))
{
  override def qlApply(context:QLContext, paramsOpt:Option[Seq[QLPhrase]] = None):QValue = {
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
      Summary("""Grabs just the first thing from the received context."""),
      Details("""    LIST -> _first -> OPTIONAL
          |Often you have a List, and you just want the first item in the List. (Especially when you
          |expect the list to only have one element in it.) Use _first to turn that List into an
          |Optional instead.
          |
          |If LIST is empty, this produces None. If LIST has elements, this produces Optional(first element).""".stripMargin)))
{
  override def qlApply(context:QLContext, paramsOpt:Option[Seq[QLPhrase]] = None):QValue = {
    val sourceColl = context.value
    val result = 
      if (sourceColl.isEmpty)
        Optional.QNone
      else
        Optional(sourceColl.cv.head)
    result
  }
}

object RestMethod extends InternalMethod(RestMethodOID,
    toProps(
      setName("_rest"),
      Summary("""Produces everything but the first thing from the received context."""),
      Details("""    LIST -> _rest -> LIST
          |Often you have a List, and you want to slice off the first item (using _first). You then use _rest
          |to handle everything else.
          |
          |_rest currently isn't useful very often. As the QL language gets more powerful, it will
          |become a useful tool, although mainly for fairly advanced programmers.""".stripMargin)))
{
  override def qlApply(context:QLContext, paramsOpt:Option[Seq[QLPhrase]] = None):QValue = {
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
  
  override def qlApply(context:QLContext, paramsOpt:Option[Seq[QLPhrase]] = None):QValue = {
    paramsOpt match {
      case Some(params) if (params.length == numParams) => {
        val urlOpt = context.value.pType match {
          case pt:URLableType => context.value.firstOpt.flatMap(pt.getURL(context)(_))
          case _ => None
        }
        
        urlOpt match {
          case Some(url) => {
            val paramTexts = params.map(phrase => context.parser.get.processPhrase(phrase.ops, context).value.wikify(context))
            HtmlValue(Html(generateButton(url, paramTexts).toString))            
          }
          // Nothing incoming, so cut.
          // TODO: there is probably a general pattern to pull out here, of "cut processing if the input is empty"
          case None => EmptyValue(RawHtmlType)
        }
      }
      case None => WarningValue(displayName + " requires " + numParams + " parameters.")
    }
  }
}

object LinkButtonMethod extends ButtonBase(LinkButtonOID,
    toProps(
      setName("_linkButton"),
      Summary("Displays a button that goes to a linked page when you press it."),
      Details("""    LINK -> _linkButton(LABEL)
          |_linkButton receives a Link or External Link, and displays that
          |link as a button. It expects one parameter, which will be the label of the button.""".stripMargin)))
{
  val numParams = 1
  
  def generateButton(url:String, params:Seq[Wikitext]):scala.xml.Elem = {
    <a class="btn btn-primary" href={url}>{params(0).raw}</a>
  }
}

object IconButtonMethod extends ButtonBase(IconButtonOID,
    toProps(
      setName("_iconButton"),
      Summary("Displays a button showing an icon, that goes to a linked page when you press it."),
      Details("""    LINK -> _iconButton(ICON, TOOLTIP)
          |_iconButton receives a Link or External Link, and displays that
          |link as a button. The first parameter identifies the icon to use for the button; the second is the
          |hover text to display as a tooltip. Both parameters are required.
          |
          |For icons, you may use anything from the [Bootstrap Glyphicon](http://getbootstrap.com/2.3.2/base-css.html#icons) set.
          |Just use the name of the icon (in double-double quotes) in the parameter.""".stripMargin)))
{
  val numParams = 2
  
  def generateButton(url:String, params:Seq[Wikitext]):scala.xml.Elem = {
    <a class="btn btn-mini btn-primary" href={url} title={params(1).raw}><i class={params(0).raw + " icon-white"}></i></a>
  }
}

// TODO: this is very similar to _linkButton, and should be refactored.
object ShowLinkMethod extends InternalMethod(ShowLinkMethodOID,
    toProps(
      setName("_showLink"),
      Summary("Displays a Link or External Link as a normal HTML link."),
      Details("""    LINK -> _showLink(LABEL)
          |This is the most normal way to display a Link or External Link with a chosen label. The
          |label may be any expression you choose.
          |
          |The default behaviour of a Link, if you don't do anything with it, is effectively
          |"_showLink(Default View)".""".stripMargin)))
{
  override def qlApply(context:QLContext, paramsOpt:Option[Seq[QLPhrase]] = None):QValue = {
    paramsOpt match {
      case Some(params) if (params.length > 0) => {
        context.value.pType match {
          case pt:URLableType => {
            context.collect(ParsedTextType) { elemContext =>
              val wikitextOpt = for (
                elemV <- elemContext.value.firstOpt;
                url <- pt.getURL(elemContext)(elemV);
                label = elemContext.parser.get.processPhrase(params(0).ops, elemContext).value.wikify(elemContext)
                  )
                yield QWikitext("[") + label + QWikitext(s"]($url)")
              
              wikitextOpt match {
                case Some(wikitext) => QValue.make(ExactlyOne, ParsedTextType, wikitext)
                case None => Optional.Empty(ParsedTextType)
              }
            }
          }
          case _ => WarningValue(displayName + " can only be used with Link types")
        }
      }
      case None => WarningValue(displayName + " requires a label parameter.")
    }
  }
}

object PropLinkMethod extends ThingPropMethod(PropLinkMethodOID, 
    toProps(
      setName("_propLink"),
      Summary("""Produces a Link to a specific Property on a Thing."""),
      Details("""    THING -> PROPERTY._propLink -> EXTERNAL LINK
          |A common pattern in Querki is to provide alternate "views" for a Thing -- different ways of displaying it.
          |Typically, you do this by creating another Large Text Property (separate from Default View), which contains
          |the alternate view, and then linking to that somewhere. This method makes it easy to do so: feed the THING
          |and PROPERTY into _propLink, and the result is an EXTERNAL LINK which you can then pass to _showLink,
          |_linkButton or _iconButton.
          |
          |NOTE: this currently only works for Things in the local Space, and probably does *not* work correctly in
          |sub-Spaces yet.
          |
          |NOTE: this does not check that the specified PROPERTY is actually a Text Property, so be careful!""".stripMargin)))
{
  def applyToPropAndThing(mainContext:QLContext, mainThing:Thing, 
    partialContext:QLContext, propErased:Property[_,_],
    params:Option[Seq[QLPhrase]]):QValue =
  {
    ExactlyOne(ExternalLinkType(mainThing.toThingId + "?prop=" + propErased.toThingId))
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
      Summary("Tests whether the provided value is non-empty"),
      Details("""    THING -> PROP._isNonEmpty
          |or
          |    RECEIVED -> _isNonEmpty
          |The first form produces true iff PROP is defined on THING, and this instance contains at least one element.
          |
          |The second form produces true iff RECEIVED contains at least one element.
          |
          |This is usually used on a List, Set or Optional, but you *can* use it on an ExactlyOne value. (In which
          |case it will always produce True.)""".stripMargin)))
{
  override def qlApply(context:QLContext, params:Option[Seq[QLPhrase]] = None):QValue = {
    boolean2YesNoQValue(!context.value.isEmpty)
  }

  def isEmpty(mainContext:QLContext, mainThing:Thing, prop:Property[_,_]):Boolean = {
    implicit val s = mainContext.state
    val isEmpty = for (
      propAndVal <- mainThing.localProp(prop)
    )
      yield propAndVal.isEmpty
    
    isEmpty.getOrElse(true)
  }
  
  def applyToPropAndThing(mainContext:QLContext, mainThing:Thing, 
    partialContext:QLContext, prop:Property[_,_],
    params:Option[Seq[QLPhrase]]):QValue =
  {
    return boolean2YesNoQValue(!isEmpty(mainContext, mainThing, prop))
  }
}

object IsEmptyMethod extends ThingPropMethod(IsEmptyOID,
    toProps(
      setName("_isEmpty"),
      Summary("Tests whether the provided value is empty"),
      Details("""    THING -> PROP._isEmpty
          |or
          |    RECEIVED -> _isEmpty
          |The first form produces true iff PROP is not defined on THING, or the value is empty.
          |
          |The second form produces true iff RECEIVED is empty.
          |
          |This is usually used on a List, Set or Optional, but you *can* use it on an ExactlyOne value. (In which
          |case it will always produce False.)""".stripMargin)))
{
  override def qlApply(context:QLContext, params:Option[Seq[QLPhrase]] = None):QValue = {
    boolean2YesNoQValue(context.value.isEmpty)
  }

  def applyToPropAndThing(mainContext:QLContext, mainThing:Thing, 
    partialContext:QLContext, prop:Property[_,_],
    params:Option[Seq[QLPhrase]]):QValue =
  {
    return boolean2YesNoQValue(IsNonEmptyMethod.isEmpty(mainContext, mainThing, prop))
  }
}

object PluralizeMethod extends InternalMethod(PluralizeOID,
    toProps(
      setName("_pluralize"),
      Summary("Produces the right word depending on how many elements are in a collection."),
      Details("""    RECEIVED -> _pluralize(SINGULAR,PLURAL)
          |This is a convenient method for choosing different text depending on a Property. The RECEIVED
          |Context should usually be a List. If it contains a single element, _pluralize produces
          |SINGULAR; if it contains multiple *or* zero elements, _pluralize produces PLURAL.
    	  |
          |Note that this behaviour is pretty English-specific. We expect that other variations will
          |be needed for other languages in the long run.""".stripMargin)))
{
  override def qlApply(context:QLContext, paramsOpt:Option[Seq[QLPhrase]] = None):QValue = {
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
      Summary("Filter out non-matching elements of a collection"),
      Details("""    RECEIVED -> _filter(FILTER)
          |This function is how you take a List of things, and whittle them down to just the ones you want.
          |
          |The FILTER should take a Thing, and produce a YesNo that says whether to include this Thing.
          |That gets applied to each element of RECEIVED; if FILTER returns Yes, then it is included, otherwise not.
    	  |
          |This is one of the most commonly-useful functions in Querki. It is how you usually say, "I only want *some*
          |of the elements in this List or Set".""".stripMargin)))
{
  override def qlApply(context:QLContext, paramsOpt:Option[Seq[QLPhrase]] = None):QValue = {
    
    // TODO: this is currently convoluted and hard to understand -- we're dissecting the list using
    // flatMapAsContext(); yielding an Option saying whether to keep each one; stitching it back together
    // as a Context, and then just using the QValue. Bleah.
    def tryElem(parser:QLParser, phrase:QLPhrase)(elem:QLContext):Option[ElemValue] = {
      val passesYesNo = parser.processPhrase(phrase.ops, elem).value
      for (
        bool <- passesYesNo.firstAs(YesNoType) if (bool);
        theElem <- elem.value.firstOpt
      ) yield theElem
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

object SpaceMethod extends SingleThingMethod(SpaceMethodOID, "_space", "What Space is this Thing in?", 
    """    RECEIVED -> _space -> SPACE
    |
    |This function produces the Space that the received Thing is contained in.""".stripMargin,
{ (thing, context) => LinkValue(thing.spaceId) })

object ExternalRootsMethod extends SingleThingMethod(ExternalRootsOID, "_externalRoots", "What are the ancestor Things for this Space?", 
    """    SPACE -> _externalRoots -> ROOTS
    |
    |Pass in a link to a Space; this produces all of the "roots" -- the Things from its Apps -- used
    |by that Space.
    |
    |User code will rarely care about this function, but it is part of how the [[All Things._self]] display works.""".stripMargin,
{ (thing, context) => QList.from(context.state.thingRoots, LinkType) })

object AllPropsMethod extends SingleThingMethod(AllPropsMethodOID, "_allProps", "What are the Properties in this Space?", 
    """    SPACE -> _allProps -> PROPS
    |
    |This receives a link to a Space, and produces all of the Properties defined in that Space.""".stripMargin,
{ (thing, context) => 
  thing match {
    case s:SpaceState => QList.from(s.propList.toSeq.sortBy(_.displayName), LinkFromThingBuilder) 
    case _ => WarningValue("_allProps must be used with a Space")
  }
  
})

object SortMethod extends InternalMethod(SortMethodOID,
    toProps(
      setName("_sort"),
      Summary("Sort the received list"),
      Details("""    LIST -> _sort -> SORTED
          |or
          |    LIST -> _sort(EXP) -> SORTED
          |With no parameters (the first form), _sort sorts the elements of the received List alphabetically by their Display Names.
          |This is what you want most of the time. However, note that many methods that return Lists are sorted to begin with,
          |so you often don't even need to bother. (Sets of Links are always sorted by Display Name.)
          |
          |If a parameter is given (the second form), it is applied to each element in LIST, and the results are used to sort the
          |elements. The sort order is whatever is natural for the returned elements -- usually alphabetical, but might be, for example,
          |numeric if the results are numeric. It is essential that EXP return the same type for all elements, and it should return
          |ExactlyOne value. (If it returns a List, only the first will be used. The behaviour is undefined if it returns a Set, or None.)
          |
          |At the moment, _sort is mainly designed for Links -- that is, pointers to Things -- since that is what 95% of use cases require. 
          |We plan to make it more general, when folks come up with use cases that need it.
          |
          |Most of the time, you will want EXP to simply be the name of a Property. For example, this:
          |
          |    My Stuff._instance -> _sort(Title)
          |
          |Produces all of the Instances of the "My Stuff" Model, sorted based on the "Title" Property. But it's possible to get much fancier if you need to:
          |EXP can be any QL Expression that receives a Link and produces a consistent Type.
          |
          |If you need to reverse the order of the sort, use the [[_desc._self]] method inside of it.
          |
          |If two or more elements being sorted have the same sort value, they will be sorted by Display Name.
          |
          |There is currently no way to define your own customized sort order. It'll probably happen someday, but will depend on user
          |demand. It is likely that we will add the ability to sort by multiple keys (sort on Property A, then Property B if those are
          |identical) in the not-too-distant future -- yell if this proves important for you.""".stripMargin)))
{
  override def qlApply(context:QLContext, paramsOpt:Option[Seq[QLPhrase]] = None):QValue = {
    
    implicit val s = context.state
    implicit val rc = context.request

    // TODO: this is awfully inefficient -- we're recomputing the processPhrase repeatedly during
    // the sort process. We should probably instead map the parameter over the list, and then
    // sort using the results:
    def thingSortFunc(left:Thing, right:Thing):Boolean = {
      val sortResult =
        for (
            params <- paramsOpt;
            // This may return a wrapped Delegating Type. (Eg, DescendingType.)
            leftCalc = context.parser.get.processPhrase(params(0).ops, context.next(ExactlyOne(LinkType(left)))).value;
            // IMPORTANT: leftResult is the ElemValue, and its pType may *not* be the same as leftCalc! The ElemValue
            // is the real element, which is likely to be of the underlying PType, without any _desc wrapper:
            leftResult <- leftCalc.firstOpt orElse { Some(leftCalc.pType.default) };
            rightCalc = context.parser.get.processPhrase(params(0).ops, context.next(ExactlyOne(LinkType(right)))).value;
            rightResult <- rightCalc.firstOpt orElse { Some(rightCalc.pType.default) };
            if (leftResult.pType.realType == rightResult.pType.realType);
            // If the two values are equal, fall through to the default:
            if (!leftCalc.pType.matches(leftResult, rightResult))
          )
          yield leftCalc.pType.comp(context)(leftResult, rightResult)

      // Default to sorting by displayName if anything doesn't work correctly:
      sortResult.getOrElse(left.displayName < right.displayName)
    }
    
    val start = context.value.cv.toSeq
    val pType = context.value.pType
    pType match {
      case LinkType => {
        // TODO: we probably don't need to translate these to Things any more:
        val asThings = start.map(elemV => context.state.anything(LinkType.get(elemV))).flatten
        val sortedOIDs = asThings.sortWith(thingSortFunc).map(_.id)
        // TODO: there is obviously a refactoring screaming to break free here, but it involves some fancy
        // type math. How do we lift things so that we can do QList.from() an arbitrary PType? (Remember that
        // it expects a PTypeBuilder, *and* requires that the input Iterable be of the expected RT.)
        QList.from(sortedOIDs, LinkType)
      }
      case _ => {
        val sorted = start.sortWith(pType.comp(context))
        QList.makePropValue(sorted, pType)
      }
    }
  }
}

/**
 * A pseudo-Type, which exists solely for the _desc method. This is a DelegatingType that is exactly like the one
 * it wraps around, except that it has a reversed sort order.
 */
class DescendingType[VT](baseType: PType[VT]) extends DelegatingType[VT](baseType) {
  override def doComp(context:QLContext)(left:VT, right:VT):Boolean = !realType.doComp(context)(left, right)
}

object DescMethod extends InternalMethod(DescMethodOID,
    toProps(
      setName("_desc"),
      Summary("Sort this list in descending order"),
      Details("""    LIST -> _sort(_desc(EXP)) -> SORTED
          |
          |_desc returns the given EXP, tweaked so that the values in it have the reversed sort order from
          |what they would normally have. It is usually used inside of _sort, to reverse the sort order, which
          |is normally in ascending order.""".stripMargin)))
{
  override def qlApply(context:QLContext, paramsOpt:Option[Seq[QLPhrase]] = None):QValue = {
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
    """    MODEL -> _children -> LIST OF CHILDREN
    |This produces all of the Things that list MODEL as their Model. It includes both other Models, and Instances.""".stripMargin,
{ (thing, context) => QList.from(context.state.children(thing).map(_.id), LinkType) })

object IsModelMethod extends SingleThingMethod(IsModelMethodOID, "_isModel", "This produces Yes if the received Thing is a Model.",
    """    THING -> _isModel -> Yes or No""".stripMargin,
{ (thing, context) => ExactlyOne(thing.isModel(context.state)) })

// TODO: this is so full of abstraction breaks it isn't funny. Using routes here is inappropriate; indeed, the fact that we're referring
// to Play at all in this level is inappropriate. This probably needs to be routed through the rendering system, so that it takes the
// current rendering environment and produces a relative control appropriate within it. But it'll do for the short term.
import controllers.routes
object CreateInstanceLinkMethod extends SingleThingMethod(CreateInstanceLinkOID, "_createInstanceLink", 
    "Given a received Model, this produces a Link to create an instance of that Model.",
    """    MODEL -> _createInstanceLink -> _linkButton(LABEL)
    |This is how you implement a "Create" button. _createInstanceLink takes a MODEL, and produces an External Link to the page to create a new Instance of it.
    |
    |You will usually then feed this into, eg, _linkButton or _iconButton as a way to display the Link.""".stripMargin,
{ (thing, context) => 
  import controllers.PlayRequestContext
  context.request match {
    case PlayRequestContext(request, _, _, _, _, _, _, _, _, _, _) => {
      implicit val req = request
      ExactlyOne(
        ExternalLinkType(routes.Application.createThing(context.request.ownerId.toThingId, context.state.toThingId, Some(thing.toThingId)).absoluteURL()))
    }
    case _ => WarningValue("_createInstanceLink does not currently work outside of Play")
  }
})

// TODO: this will become clearer and easier to use once we introduce block-syntax parameters.
object IfMethod extends InternalMethod(IfMethodOID,
    toProps(
      setName("_if"),
      Summary("Choose what to produce, as directed"),
      Details("""    RECEIVED -> _if(YESNO, IFCLAUSE, ELSECLAUSE) -> ...
          |_if is one of the basic building blocks of programming. It applies the YESNO phrase to the received context.
          |If the result is Yes, it applies the IFCLAUSE to the received context and produces that. Otherwise, if there
          |is an ELSECLAUSE, it applies and produces that, or produces None if there is no ELSECLAUSE.
          |
          |The syntax of _if is likely to evolve in the future, to something more like most programming languages. For now,
          |though, note that IFCLAUSE and ELSECLAUSE are *inside* the parentheses, rather than after them as most languages
          |have it.""".stripMargin)))
{
  override def qlApply(context:QLContext, paramsOpt:Option[Seq[QLPhrase]] = None):QValue = {
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
      Summary("Combine a list of Text values together"),
      Details("""    LIST -> _join(OPEN, SEP, CLOSE) -> QTEXT
          |_join takes the given LIST, and turns it into a single line. For example, if My List was "Cat", "Dog", "Horse",
          |then
          |    My List -> _join
          |would come out as "CatDogHorse".
          |
          |Of course, that probably isn't what you want -- most of the time, you want some separators at the beginning,
          |middle and end. Those are the parameters; how many parameters you give define how they are used. If there is
          |only one, then it is SEP, the separator in between elements. So
          |    My List -> _join(", ")
          |would come out as "Cat, Dog, Horse" -- more reasonable.
          |
          |If there are two parameters, then they are OPEN and SEP. So for example, if I wanted to include dashes at the
          |beginning, that would be:
          |    My List -> _join("-- ", ", ")
          |which would come out as "-- Cat, Dog, Horse". And if I wanted parentheses around the entire list, I'd use all
          |three parameters -- OPEN, SEP and CLOSE -- as:
          |    My List -> _join("(", ", ", ")")
          |to get "(Cat, Dog, Horse)".
          |
          |Note that you can use _join with anything, not just Text -- if the received values aren't Text, then they will
          |be rendered into their default forms before getting combined. But at the end of _join, what you get back is
          |one big block of QText. You can't do any further processing on the elements after this.""".stripMargin)))
{
  override def qlApply(context:QLContext, paramsOpt:Option[Seq[QLPhrase]] = None):QValue = {
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
          val renderedParam = paramVal.pType.wikify(context)(paramVal.first)
          renderedParam
        }
        case _ => Wikitext.empty
      }
    }

    val elemT = context.value.pType
    val renderedList = context.value.cv.map{elem => elemT.wikify(context)(elem)}
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
      Summary("Produces a List of all Things that have the received Thing or Name as a Tag"),
      Details("""    NAME -> _tagRefs -> THINGS
          |_tagRefs is usually the right way to answer the question "what points to this?" For example, if I wanted to
          |show a bullet list of everything that points to the current Thing, I would simply say:
          |    _tagRefs -> _bulleted
          |_tagRefs is designed to receive a "name" (which is what a Tag is). If you send it a Thing, then it will use
          |the Name of that Thing.
          |
          |NOTE: _tagRefs and _refs are closely related concepts. They currently work differently, but we might
          |at some point combine them for simplicity.""".stripMargin)))
{ 
  override def qlApply(context:QLContext, paramsOpt:Option[Seq[QLPhrase]] = None):QValue = {
    val elemT = context.value.pType
    elemT match {
      case nameable:NameableType => {
        val allProps = context.state.allProps.values
        val tagProps = allProps.filter(prop => prop.pType == TagSetType || prop.pType == NewTagSetType)
        val name = nameable.getName(context)(context.value.first)
        val thingOpt = elemT match {
          case LinkType => LinkType.followLink(context)
          case _ => None
        }
        val namePt = thingOpt.map(thing => PlainText(thing.unsafeDisplayName)).getOrElse(PlainText(name))
        val candidates = context.state.allThings
        
        def hasThisTag(candidate:Thing):Boolean = {
          tagProps.exists{ prop =>
            val propAndVal = candidate.localProp(prop)
            val found = prop.pType match {
              case TagSetType => {
	            val candidateTags:Option[List[String]] = propAndVal.map(_.v.rawList(TagSetType))
	            candidateTags.map(_.exists { candidateName => NameType.equalNames(candidateName, name) })
              }
              case NewTagSetType => {
	            val candidateTags:Option[List[PlainText]] = propAndVal.map(_.v.rawList(NewTagSetType))
	            candidateTags.map(_.exists { candidateName => NewTagSetType.equalNames(candidateName, namePt) })                
              }
            }
            found.getOrElse(false)
          }
        }
        
        QList.from(candidates.filter(hasThisTag), LinkFromThingBuilder)
      }
      case _ => WarningValue("_tagRefs can only be used with a Tag or Link, not " + elemT.displayName)
    }
  }
}

object TagsForPropertyMethod extends SingleContextMethod(TagsForPropertyOID,
    toProps(
      setName("_tagsForProperty"),
      Summary("Show all the Tags that are defined for this Property"),
      // TODO: this isn't displaying properly. Why not? It looks like the "" nested inside of the indirect
      // Property is causing the problem -- I am getting a syntax error *claiming* to be in Default View,
      // pointing at the first "":
      Details("""    TAG PROPERTY._tagsForProperty -> LIST OF TAGS
          |_tagsForProperty can be used on any Property whose Type is Tag Set. It produces a list of all of the
          |tags that have been used in that Property so far.
          |
          |Typically, you then feed the results of this to _tagRefs, to get the Things that use that Tag. For example,
          |if I had a list of Wines, using a Tag Set Property giving its "Wine Color", I could say:
          |    \[[Wine Color._tagsForProperty -> \""* \____: \[[_tagRefs -> _commas\]]\""\]]
          |to produce a list like:
          |* Red: Pinot Noir, Shiraz
          |* White: Pinot Gris, Chardonnay
          |""".stripMargin)))
{
  def fetchTags(space:SpaceState, propIn:Property[_,_]):Set[String] = {
    implicit val s = space
    val thingsWithProp = space.thingsWithProp(propIn)
    if (propIn.pType == TagSetType) {
      val prop = propIn.confirmType(TagSetType)
      (Set.empty[String] /: thingsWithProp) { (set, thing) =>
        set ++ thing.getProp(prop).rawList
      }
    } else if (propIn.pType == NewTagSetType) {
      val prop = propIn.confirmType(NewTagSetType)
      (Set.empty[String] /: thingsWithProp) { (set, thing) =>
        set ++ thing.getProp(prop).rawList.map(_.text)
      }      
    } else
      // TODO: should this be a PublicException?
      throw new Exception("Trying to fetchTags on a non-Tag Property!")
  }
  
  def fullyApply(mainContext:QLContext, partialContext:QLContext, params:Option[Seq[QLPhrase]]):QValue = {
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
      Summary("Get a Link to this Thing"),
      Details("""*thing*._self simply produces *thing*.
          |
          |This seems silly, but it is useful for overriding the usual _apply behavior. In particular,
          |*property*._self is the way to get a link to the property itself, instead of fetching the value
          |of the property on the received Thing.
          |
          |More formally, _self is the way to override the usual [[_apply]] behaviour on a Thing, to get a
          |Link to that Thing. It is never necessary for ordinary Things, but frequently useful when _apply
          |has been defined on it.""".stripMargin)))
{
  def fullyApply(mainContext:QLContext, partialContext:QLContext, params:Option[Seq[QLPhrase]]):QValue = {
    partialContext.value
  }
}

object PropsOfTypeMethod extends SingleThingMethod(PropsOfTypeOID, "_propsOfType", "This receives a Type, and produces all of the Properties in this Space with that Type",
    """    TYPE -> _propsOfType -> LIST OF PROPS""".stripMargin,
{ (thing, context) =>
  thing match {
    case pt:PType[_] => QList.from(context.state.propsOfType(pt), LinkFromThingBuilder)
    case _ => WarningValue("_propsOfType can only be used on a Type")
  }
})

object CodeMethod extends SingleContextMethod(CodeMethodOID,
    toProps(
      setName("_code"),
      Summary("Display a block of QL code"),
      Details("""_code() displays the raw code of a value or property, pretty flexibly.
          |
          |You can give it as "TEXT -> _code" to display the TEXT -- however, note that the TEXT will be processed as normal
          |in this case. If you want to show some raw code, unprocessed, do it as "_code(TEXT)" instead.
          |
          |You can give a property as a parameter -- "_code(PROP)" -- and it will display the value of the property on this Thing.
          |
          |Or you can give a property on some other Thing -- "_code(THING.PROP)" -- to display the value of the property on that Thing.
          |
          |If you have a parameter, and it doesn't work as either PROP or THING.PROP, then it will display the parameter literally.
          |
          |The results are displayed in an inset block, in monospaced type, so that it looks "codish".
          |
          |_code is, frankly, a bit persnickety at this point, and not always easy to use for complicated examples. It should be
          |considered a work in progress.""".stripMargin)))
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
  def fullyApply(mainContext:QLContext, partialContext:QLContext, paramsOpt:Option[Seq[QLPhrase]]):QValue = {
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
          case QLBinding(_) => WarningValue("It is meaningless to call _code on a Binding.")
          case QLCall(name, methodNameOpt, _, _) => {
            val thingName = name.name
            methodNameOpt match {
              case Some(methodName) => {
                val resultOpt = for (
                  thing <- space.anythingByName(thingName);
                  propThing <- space.anythingByName(methodName);
                  encoded <- encodeThingAndProp(thing, propThing)
                )
                  yield encoded
                  
                resultOpt.getOrElse(encodeString(phrase.reconstructString))
              }
              case None => {
                val propOpt = space.anythingByName(thingName)
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
      Summary("Produces Yes if the name passed into it is a real Thing"),
      Details("""    NAME -> _isDefined -> YES or NO
          |You typically use _isDefined with a Tag Property. It is simply a way to ask "is there actually something
          |with this name?", so that you can handle it differently depending on whether there is or not.""".stripMargin)))
{
  def fullyApply(mainContext:QLContext, partialContext:QLContext, paramsOpt:Option[Seq[QLPhrase]]):QValue = {
    partialContext.value.pType != UnknownNameType
  }
}

object CountMethod extends SingleContextMethod(CountMethodOID,
    toProps(
      setName("_count"),
      Summary("Produces the number of elements in the received Collection"),
      Details("""    LIST -> _count -> NUMBER
          |This is pretty much as simple as it sounds. It is most often used in the header of a _section, like this:
          |    \[[My List -> _section(\""Items: (\[[_count\]])\"", _commas)\]]""".stripMargin)))
{
  def fullyApply(mainContext:QLContext, partialContext:QLContext, paramsOpt:Option[Seq[QLPhrase]]):QValue = {
    ExactlyOne(IntType(partialContext.value.cv.size))
  }
}

// TODO: this code is pretty damned Bootstrap-specific, which by definition is too HTML-specific. We should probably
// replace it with something that is much more neutral -- simple label/control styles -- and have client-side code
// that rewrites it appropriately for the UI in use.
object FormLineMethod extends SingleContextMethod(FormLineMethodOID,
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

object ReverseMethod extends SingleContextMethod(ReverseMethodOID,
    toProps(
      setName("_reverse"),
      Summary("Produces the same Collection it receives, as a List, in reverse order"),
      Details("""    LIST -> _reverse -> REVERSED LIST
          |
          |This does exactly what it sounds like: it produces the same list, in reversed order.
          |
          |_reverse can technically be used on any Collection, but is only useful for Lists. However,
          |it can be useful after sorting a Set:
          |
          |    SET -> _sort -> _reverse
          |
          |You can't _reverse a Set itself (Sets have their own intrinsic order), but _sort always
          |produces a List.""".stripMargin)))
{
  def fullyApply(mainContext:QLContext, partialContext:QLContext, paramsOpt:Option[Seq[QLPhrase]]):QValue = {
    QList.makePropValue(partialContext.value.cv.toSeq.reverse.toList, partialContext.value.pType)
  }
}

object OIDMethod extends SingleThingMethod(OIDMethodOID, "_oid", "Get the unique global id of this Thing", 
    """    THING -> _oid -> Text
    |
    |This function produces the unique Object ID (which will generally be a period followed by some letters and numbers)
    |of the received Thing.
    |
    |Each Thing in Querki has an Object ID. In most cases, it can be used in place of the Thing's name, and it is never
    |ambiguous -- it always refers to one specific Thing.""".stripMargin,
{ (thing, context) => TextValue(thing.id.toThingId) })

object KindMethod extends InternalMethod(KindMethodOID,
    toProps(
      setName("_kind"), 
      Summary("What kind of Thing is this?"), 
      Details("""There are two ways to use _kind:
          |    THING -> _kind -> Number
          |
          |This function produces the Number that represents the "kind"
          |of the received Thing. The Kinds are:
          |
          |* Thing: 0
          |* Type: 1
          |* Property: 2
          |* Space: 3
          |* Collection: 4
          |* Attachment: 5
          |
          |By and large, though, you should never use these numbers directly. Instead, use
          |the second form of this method:
          |    _kind(KIND) -> Number
          |The KIND parameter should be exactly one of the above names.
          |
          |So for example, you can test whether the incoming Thing is a Property by saying:
          |    ... -> _if(_equals(_kind, _kind(Property)), ...)
          |""".stripMargin)))
{ 
  override def qlApply(context:QLContext, paramsOpt:Option[Seq[QLPhrase]] = None):QValue = {
    // If there is a parameter, this will produce its value:
    val paramResult = for (
      params <- paramsOpt;
      param = params(0);
      QLCall(kindName, _, _, _) = param.ops(0)
        )
      yield Kind.fromName(kindName.name).map(kind => ExactlyOne(IntType(kind))).getOrElse(WarningValue("Unknown Kind: " + kindName))
      
    // If not, produce the incoming Thing's value:
    paramResult.getOrElse(applyToIncomingThing(context) { (thing, context) => ExactlyOne(IntType(thing.kind)) })
  }
}

object CurrentSpaceMethod extends SingleThingMethod(CurrentSpaceMethodOID, "_currentSpace", "What Space are we looking at?", 
    """THING -> _currentSpace -> SPACE
    |
    |This function produces the Space that we are currently displaying. (Generally, the one in the URL.)""".stripMargin,
{ (thing, context) => LinkValue(context.root.state) })

object IsMethod extends InternalMethod(IsMethodOID,
    toProps(
      setName("_is"),
      Summary("Allows you to test whether you have a specific Thing"),
      Details("""    THING -> _is(THING) -> Yes or No
    |
    |This function produces Yes iff the parameter matches the passed-in THING, and No otherwise. It is almost always used
    |inside _if(). For instance, to check whether a Property is of Text Type:
    |    MyProp.Property Type -> _if(_is(Text Type), ...)""".stripMargin)))
{ 
  override def qlApply(context:QLContext, paramsOpt:Option[Seq[QLPhrase]] = None):QValue = {
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

object EqualsMethod extends InternalMethod(EqualsMethodOID,
    toProps(
      setName("_equals"),
      Summary("Do these parameters match?"),
      Details("""    _equals(EXP1, EXP2) -> YES OR NO
          |_equals produces Yes iff the expressions in the two parameters match each other. The definition
          |of "match" is type-dependent, but by and large is similar to == in most programming languages.
          |
          |Note that we are likely to enhance this method in the future, to do more, but this is the
          |important core functionality.""".stripMargin)))
{  
  override def qlApply(context:QLContext, paramsOpt:Option[Seq[QLPhrase]] = None):QValue = {
    paramsOpt match {
      case Some(params) if (params.length > 1) => {
        val first = context.parser.get.processPhrase(params(0).ops, context).value
        val second = context.parser.get.processPhrase(params(1).ops, context).value
        if (first.pType != second.pType) {
          WarningValue("The parameters to _equals must be the same Type: got _equals(" + first.pType.displayName + ", " + second.pType.displayName + ")")
        } else if (first.size == second.size) {
          val pt = first.pType
          val pairs = first.cv.zip(second.cv)
          pairs.forall(pair => pt.matches(pair._1, pair._2))
        } else {
          false
        }
      }
      case _ => WarningValue("_equals requires two parameters")
    }
  }
}