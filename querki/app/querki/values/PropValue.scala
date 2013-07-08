package querki.values

import models._

import ql.ContextBase

/**
 * The central representation of a value of a property.
 * 
 * TODO: this does not inherently know the PType of the Property, nor the VT. That's a pain in the
 * butt. Can we fix it without tying ourselves in knots? Note that we *CANNOT* have this include the
 * Property itself -- this is used for intermediate values that aren't actually from Properties --
 * but it could incorporate the actual PType.
 */
trait PropValue {
  type cType = coll.implType
  type pType = PType[_]
  
  val coll:Collection
  def cv:cType
  
  def serialize(elemT:pType):String = coll.doSerialize(cv, elemT)
  def first = coll.first(this)
  def firstTyped[VT](elemT:PType[VT]):Option[VT] = if (isEmpty) None else Some(elemT.get(first))
  def render(context:ContextBase, elemT:pType):Wikitext = coll.doRender(context)(cv, elemT)
  
  def isEmpty = coll.isEmpty(this)
  def size = cv.size
  
  def flatMap[VT, T](elemT:PType[VT])(cb:VT => Option[T]) = cv.flatMap { elem => 
    val vt = elemT.get(elem)
    cb(vt)
  }
  
  def rawList[VT](elemT:PType[VT]):List[VT] = {
    (List.empty[VT] /: cv) ((list, elem) => list :+ elemT.get(elem))
  }
}
