package querki.values

import models._

import models.system.QList

/**
 * A convenient wrapper for passing a value around in a way that can be fetched.
 */
case class PropAndVal[VT](prop:Property[VT, _], v:PropValue) {
  def render(context:ContextBase) = prop.render(context)(v)
  def renderPlain = render(EmptyContext)
  def renderOr(context:ContextBase)(other: => Wikitext) = if (prop.isEmpty(v)) other else render(context)
  def renderPlainOr(other: => Wikitext) = renderOr(EmptyContext)(other)
  def renderPlainIfDefined = if (!prop.isEmpty(v)) renderPlain else Wikitext("")
  def split() = (prop, v)
  def first = prop.first(v)
  def flatMap[T](cb:VT => Option[T]) = prop.flatMap(v)(cb)
  def ++(others:Iterable[VT]):PropValue = {
    QList.makePropValue((v.cv ++ others.map(ElemValue(_, prop.pType))).toList)
  }
  def contains(toCheck:VT):Boolean = prop.contains(v, toCheck)
  def isEmpty:Boolean = v.isEmpty
  def rawList:List[VT] = v.rawList(prop.pType)
}

