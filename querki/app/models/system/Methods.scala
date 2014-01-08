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

