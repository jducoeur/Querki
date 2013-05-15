package models.system

import play.api.Logger

import models._
import Property._
import Thing._

import ql._

import OIDs._

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
  override def qlApply(context:ContextBase, params:Option[Seq[ContextBase]] = None):TypedValue = {
    // By default, we just pass the incoming context right through:
    return context.value
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
  override def qlApply(context:ContextBase, params:Option[Seq[ContextBase]] = None):TypedValue = {
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

class PartiallyAppliedFunction(partialContext:ContextBase, action:(ContextBase, Option[Seq[ContextBase]]) => TypedValue) extends QLFunction {
  def qlApply(context:ContextBase, params:Option[Seq[ContextBase]] = None):TypedValue = {
    action(context, params)
  }
}
/**
 * A MetaMethod is a Method that is intended to be dotted -- that is, it should be specified on the
 * right-hand side of a dot. _edit is the canonical example. Usually, these are methods on a Property,
 * which need to be contextualized by the Thing that Property applies to.
 * 
 * TBD: this is a pretty weak version of partial application. At some point, let's see if we can
 * refactor this to be more general, powerful and correct. Also, the interaction of this and
 * PartiallyAppliedFunction is clearly too baroque, and can probably be simplified.
 */
abstract class MetaMethod(tid:OID, p:PropFetcher) extends InternalMethod(tid, p)
{
  override def qlApply(context:ContextBase, params:Option[Seq[ContextBase]] = None):TypedValue = {
    ErrorValue(displayName + " can not be applied on its own; you need to use this on the right-hand side of a dot, as PropertyName." + displayName)
  }
  
  override def partiallyApply(leftContext:ContextBase):QLFunction = {
    def handleRemainder(mainContext:ContextBase, params:Option[Seq[ContextBase]]):TypedValue = {
      fullyApply(mainContext, leftContext, params)
    }
    new PartiallyAppliedFunction(leftContext, handleRemainder)
  }
  
  /**
   * The actual Method must implement this. It takes both contexts -- the partial context that we were
   * dotted to and the main incoming context -- and does the usual sorts of things with them.
   */
  def fullyApply(mainContext:ContextBase, partialContext:ContextBase, params:Option[Seq[ContextBase]]):TypedValue
}

object EditMethod extends MetaMethod(EditMethodOID, 
    toProps(
      setName("_edit"),
      DisplayTextProp("Puts an editor for the specified Property into the page"),
      (AppliesToKindOID -> QList(ElemValue(Kind.Property)))
    )) 
{
  def fullyApply(mainContext:ContextBase, partialContext:ContextBase, params:Option[Seq[ContextBase]]):TypedValue = {
    applyToIncomingThing(mainContext) { (mainThing, _) =>
      applyToIncomingThing(partialContext) { (shouldBeProp, _) =>
        shouldBeProp match {
          case prop:Property[_,_] => {
            val currentValue = mainThing.getDisplayPropVal(prop)(mainContext.state)
            // TODO: conceptually, this is a bit off -- the rendering style shouldn't be hard-coded here. We
            // probably need to have the Context contain the desire to render in HTML, and delegate to the
            // HTML renderer indirectly. In other words, the Context should know the renderer to use, and pass
            // that into here:
            val inputControl = querki.html.HtmlRenderer.renderPropertyInput(mainContext.state, prop, currentValue)
            HtmlValue(inputControl)
          }
          case _ => ErrorValue("The _edit method can only be used on Properties")
        } 
      }
    }
  }
}