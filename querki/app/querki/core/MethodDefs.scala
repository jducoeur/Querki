package querki.core

import querki.ecology._

import querki.ql.{PartiallyAppliedFunction, QLFunction, QLPhrase}

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
    override def qlApply(inv:Invocation):QValue = {
      // By default, we just pass the incoming context right through:
      inv.context.value
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
    override def qlApply(inv:Invocation):QValue = {
      val context = inv.context
      
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
}