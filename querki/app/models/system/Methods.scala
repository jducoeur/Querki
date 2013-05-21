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

class PartiallyAppliedFunction(partialContext:ContextBase, action:(ContextBase, Option[Seq[QLPhrase]]) => TypedValue) extends QLFunction {
  def qlApply(context:ContextBase, params:Option[Seq[QLPhrase]] = None):TypedValue = {
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

object EditMethod extends MetaMethod(EditMethodOID, 
    toProps(
      setName("_edit"),
      DisplayTextProp("Puts an editor for the specified Property into the page"),
      (AppliesToKindOID -> QList(ElemValue(Kind.Property)))
    )) 
{
  def fullyApply(mainContext:ContextBase, partialContext:ContextBase, params:Option[Seq[QLPhrase]]):TypedValue = {
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