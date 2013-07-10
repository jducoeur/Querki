package querki.values

import models._

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
  
  def pType:PType[_]
  
  val coll:Collection
  def cv:cType
  
  // TODO: this doesn't need to take elemT any more:
  def serialize(elemT:PType[_]):String = coll.doSerialize(cv, elemT)
  def first = coll.first(this)
  def firstTyped[VT](elemT:PType[VT]):Option[VT] = if (isEmpty) None else Some(elemT.get(first))
  def render(context:ContextBase):Wikitext = coll.doRender(context)(cv, pType)
  
  def isEmpty = coll.isEmpty(this)
  def size = cv.size
  
  // TODO: these generally don't need the elemT any more; replace with a general as[VT](elemT) method
  // that does the cast for when we actually give a damn:
  def flatMap[VT, T](elemT:PType[VT])(cb:VT => Option[T]) = cv.flatMap { elem => 
    val vt = elemT.get(elem)
    cb(vt)
  }
  
  def rawList[VT](elemT:PType[VT]):List[VT] = {
    (List.empty[VT] /: cv) ((list, elem) => list :+ elemT.get(elem))
  }
}
