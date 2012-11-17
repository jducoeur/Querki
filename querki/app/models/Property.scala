package models

import Thing._

import system.SystemSpace._

/**
 * The value of a primitive Type. These are always considered "elements", since they
 * are always wrapped inside Collections.
 * 
 * Note that ElemValue is untyped. The is necessary -- if we try to keep this strongly
 * typed, then the matrix composition of Collections and PTypes becomes impossible at
 * runtime. So ElemValues are fundamentally not typesafe, and should only be evaluated
 * in the context of their associated PTypes.
 */
case class ElemValue(elem:Any)

case class PropValue[CT](coll:CT)

/**
 * Properties have Types. There's nothing controversial here -- Types are usually
 * things like Text, Number and so on. But note that Types are themselves Things;
 * this is specifically so that we can potentially add user-defined Types down
 * the road.
 */
abstract class PType[VT](i:OID, s:ThingPtr, m:ThingPtr, pf:PropFetcher) extends Thing(i, s, m, Kind.Type, pf) {
  
  type valType = VT
  
  /**
   * Each PType is required to implement this -- it is the deserializer for the
   * type.
   */
  protected def doDeserialize(ser:String):VT
  final def deserialize(ser:String):ElemValue = ElemValue(doDeserialize(ser))
  
  /**
   * Also required for all PTypes, to serialize values of this type.
   */
  protected def doSerialize(v:VT):String
  final def serialize(v:ElemValue):String = doSerialize(get(v))
  
  /**
   * Takes a value of this type, and turns it into displayable form. Querki
   * equivalent to toString.
   */
  protected def doRender(v:VT):Wikitext
  final def render(v:ElemValue):Wikitext = doRender(get(v))
  
  /**
   * Also required for all PTypes -- the default value to fall back on.
   */
  protected def doDefault:VT
  final def default:ElemValue = ElemValue(doDefault)
  
  /**
   * The type unwrapper -- takes an opaque ElemValue and returns the underlying value.
   * This is a fundamentally unsafe operation, so it should always be performed in the
   * context of a Property.
   */
  def get(v:ElemValue):VT = v.elem.asInstanceOf[VT]
}
trait PTypeBuilder[VT, RT] {
  def wrap(raw:RT):VT
  def apply(raw:RT):ElemValue = ElemValue(wrap(raw))  
}
trait SimplePTypeBuilder[VT] extends PTypeBuilder[VT, VT] {
  def wrap(raw:VT) = raw
}

/**
 * A Collection is the Querki equivalent of a Functor in Category Theory. Properties
 * always combine a Type *and* a Collection. (You must explicitly state both Optional
 * or ExactlyOne.) 
 * 
 * As of this writing, it isn't obvious that Collections will be
 * required to be strictly monadic, but they probably all are. By being rigorous
 * and consistent about this, we make it much easier to write QL safely -- each
 * QL step is basically a flatMap.
 */
abstract class Collection[CT](i:OID, s:ThingPtr, m:ThingPtr, pf:PropFetcher) extends Thing(i, s, m, Kind.Collection, pf) {
  
  type pType = PType[_]
  type implType = CT
  
  /**
   * Each Collection is required to implement this -- it is the deserializer for the
   * type.
   */
  def deserialize(ser:String, elemT:pType):PropValue[implType]
  
  /**
   * Also required for all Collections, to serialize values of this type.
   */
  def serialize(v:PropValue[implType], elemT:pType):String
  
  /**
   * Takes a value of this type, and turns it into displayable form. Querki
   * equivalent to toString.
   */
  def render(v:PropValue[implType], elemT:pType):Wikitext
  
  /**
   * Also required for all Collections -- the default value to fall back on.
   */
  def default(elemT:pType):PropValue[implType]
  
  def wrap(elem:ElemValue):implType
  /**
   * Convenience wrapper for creating in-code PropValues.
   * 
   * TODO: this really isn't right -- Collection shouldn't ever create ElemValues.
   */
  def apply(elem:Any):PropValue[implType] = PropValue(wrap(ElemValue(elem)))
  
  /**
   * Returns the head of the collection.
   * 
   * NOTE: this will throw an exception if you call it on an empty collection! It is the
   * equivalent of Option.get
   */
  def first(pv:PropValue[implType]):ElemValue
}

/**
 * A Property is a field that may exist on a Thing. It is, itself, a Thing with a
 * specific Type.
 */
case class Property[VT, CT](i:OID, s:ThingPtr, m:ThingPtr, val pType:PType[VT], val cType:Collection[CT], pf:PropFetcher)
  extends Thing(i, s, m, Kind.Property, pf) {
  def default = {
    // TODO: add the concept of the default meta-property, so you can set it
    // on a prop-by-prop basis
    cType.default(pType)
  }
  def defaultPair = PropAndVal(this, default)
  // EVIL but arguably necessary. This is where we are trying to confine the cast from something
  // we get out of the PropMap (which is a bit undertyped) to match the associated Property.
  def castVal(v:PropValue[_]) = v.asInstanceOf[PropValue[CT]]
  def pair(v:PropValue[_]) = PropAndVal(this, castVal(v))

  override lazy val props:PropMap = propFetcher() + 
		  CollectionProp(cType.id) +
		  TypeProp(pType.id)
  
  def render(v:PropValue[CT]) = cType.render(v, pType)
  
  def from(m:PropMap):PropValue[CT] = castVal(m(this))
  
  /**
   * Convenience method to fetch the value of this property in this map.
   */
  def first(m:PropMap):VT = pType.get(cType.first(from(m)))

  // TODO: This should actually be taking an RT, not a VT. We may need to reintroduce the notion
  // of a PType's builder into Property.
  def apply(elem:VT) = (this, cType(elem))
  
  def serialize(v:PropValue[_]):String = cType.serialize(castVal(v), pType)
  def deserialize(str:String):PropValue[cType.implType] = cType.deserialize(str, pType)
}

/**
 * A convenient wrapper for passing a value around in a way that can be fetched.
 */
case class PropAndVal[VT, CT](prop:Property[VT, CT], v:PropValue[CT]) {
  def render = prop.render(v)
  def split() = (prop, v)
}