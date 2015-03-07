package querki.values

import models._

import querki.ecology._

/**
 * A convenient wrapper for passing a value around in a way that can be fetched.
 * 
 * At this point, this is a *very* thin wrapper around QValue. I'm leaving it here for the time
 * being because it carries VT with it. So if you've fetched the QValue from a particular Property,
 * PropAndVal preserves the type, which can make it easier to work with.
 * 
 * But don't get cute with this. It is and should remain nothing more than a convenience for
 * handling things in a way that makes the Scala compiler happier for simple operations.
 * 
 * TBD: is there a better way to do this? Can we construct a variant of QValue that carries its
 * own VT? Or make this wrapper no longer require the Property, just carry the VT itself?
 */
case class PropAndVal[VT](prop:Property[VT, _], v:QValue) extends EcologyMember {
  implicit def ecology:Ecology = prop.ecology
  
  def render(context:QLContext, lexicalThing:Option[PropertyBundle] = None) = v.wikify(context, lexicalThing = lexicalThing)
  def renderPlain = render(EmptyContext(ecology))
  def renderOr(context:QLContext)(other: => Wikitext) = if (v.isEmpty) other else render(context)
  def renderPlainOr(other: => Wikitext) = renderOr(EmptyContext(ecology))(other)
  def renderPlainIfDefined = if (!v.isEmpty) renderPlain else Wikitext("")
  // TODO: Evil! Deprecated in favor of firstOpt:
  def first = v.firstAs(prop.pType).get
  def firstOpt = v.firstAs(prop.pType)
  def flatMap[T](cb:VT => Option[T]) = v.flatMap(prop.pType)(cb)
  def map[DT, RT](destType:PType[DT] with PTypeBuilder[DT, RT])(cb:VT => RT) = v.map(prop.pType, destType)(cb)
  // TODO: Evil! This should be smarter about how it combines Collections, instead of forcing things to QList.
  // In particular, combining two QSets should wind up with QSet semantics:
  def ++(others:Iterable[VT]):QValue = {
    interface[querki.core.Core].makeListValue((v.cv ++ others.map(ElemValue(_, prop.pType))).toList, prop.pType)
  }
  def contains(toCheck:VT):Boolean = v.contains(prop.pType, toCheck)
  def exists(check:VT => Boolean):Boolean = v.exists(prop.pType, check)
  def isEmpty:Boolean = v.isEmpty
  def rawList:List[VT] = v.rawList(prop.pType)
}

