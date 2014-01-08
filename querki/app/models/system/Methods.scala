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

object PropsOfTypeMethod extends SingleThingMethod(PropsOfTypeOID, "_propsOfType", "This receives a Type, and produces all of the Properties in this Space with that Type",
    """    TYPE -> _propsOfType -> LIST OF PROPS""".stripMargin,
{ (thing, context) =>
  thing match {
    case pt:PType[_] => QList.from(context.state.propsOfType(pt), LinkFromThingBuilder)
    case _ => WarningValue("_propsOfType can only be used on a Type")
  }
})

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
