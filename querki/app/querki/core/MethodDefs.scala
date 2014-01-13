package querki.core

import querki.ecology._

import ql.{QLFunction, QLPhrase}

import querki.util.QLog
import querki.values.QLContext

/**
 * Ecots that define "methods" (what we're now generally calling "functions") should mix this in.
 */
trait MethodDefs { self:QuerkiEcot =>
  
  // Since Methods usually declare Summaries, we need to depend on Conventions:
  val Conventions = initRequires[querki.conventions.Conventions]
  
  lazy val QUnit = Core.QUnit
  
  /**
   * Internal methods -- functions defined in-code that can be assigned as properties -- should
   * inherit from this.
   */
  class InternalMethod(tid:OID, p:PropFetcher)
    extends SystemProperty[String,String](tid, Core.InternalMethodType, QUnit, () => (p() + (querki.datamodel.MOIDs.IsFunctionOID  -> ExactlyOne(YesNoType(true)))))
  {
    /**
     * Methods should override this to implement their own functionality.
     */
    override def qlApply(context:QLContext, params:Option[Seq[QLPhrase]] = None):QValue = {
      // By default, we just pass the incoming context right through:
      context.value
    }
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
        case error:Exception => QLog.error("Error while running internal method", error)
        interface[querki.ql.QL].ErrorValue("Error while running internal method")
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
      interface[querki.ql.QL].WarningValue(displayName + " can not be applied on its own; you need to use this on the right-hand side of a dot, as PropertyName." + displayName)
    }
  
    override def partiallyApply(leftContext:QLContext):QLFunction = {
      def handleRemainder(mainContext:QLContext, params:Option[Seq[QLPhrase]]):QValue = {
        fullyApply(mainContext, leftContext, params)
      }
      new ql.PartiallyAppliedFunction(leftContext, handleRemainder)
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
            case _ => interface[querki.ql.QL].WarningValue("The " + displayName + " method can only be used on Properties")
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
}