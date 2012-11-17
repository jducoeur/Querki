package models

import Thing._

import system.SystemSpace._

/**
 * The value of a primitive Type. These are always considered "elements", since they
 * are always wrapped inside Collections.
 */
case class ElemValue[VT, T <: PType[VT]#valType](elem:T)

case class PropValue[CT](v:CT)

/**
 * Properties have Types. There's nothing controversial here -- Types are usually
 * things like Text, Number and so on. But note that Types are themselves Things;
 * this is specifically so that we can potentially add user-defined Types down
 * the road.
 */
abstract class PType[VT](i:OID, s:ThingPtr, m:ThingPtr, pf:PropFetcher) extends Thing(i, s, m, Kind.Type, pf) {
  
  type valType = VT
  type elemVT = ElemValue[VT, valType]
  
  /**
   * Each PType is required to implement this -- it is the deserializer for the
   * type.
   */
  def deserialize(ser:String):elemVT
  
  /**
   * Also required for all PTypes, to serialize values of this type.
   */
  def serialize(v:elemVT):String
  
  /**
   * Takes a value of this type, and turns it into displayable form. Querki
   * equivalent to toString.
   */
  def render(ser:elemVT):Wikitext
  
  /**
   * Also required for all PTypes -- the default value to fall back on.
   */
  def default:elemVT
}
trait PTypeBuilder[VT, RT] {
  def wrap(raw:RT):VT
  def apply(raw:RT):ElemValue[VT, VT] = ElemValue(wrap(raw))  
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
abstract class Collection[VT, CT](i:OID, s:ThingPtr, m:ThingPtr, pf:PropFetcher) extends Thing(i, s, m, Kind.Collection, pf) {
  
  type pType = PType[VT]
  type elemVT = ElemValue[VT, VT]
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
  
  def wrap(elem:ElemValue[VT, pType#valType]):implType
  /**
   * Convenience wrapper for creating in-code PropValues.
   */
  def apply(elem:VT):PropValue[implType] = PropValue(wrap(ElemValue(elem)))
  
  def get[ElemT](pv:PropValue[implType]):ElemT =
    throw new Exception("Tried to call Collection.get on a collection that doesn't support it!")
}

/**
 * A Property is a field that may exist on a Thing. It is, itself, a Thing with a
 * specific Type.
 */
case class Property[VT, CT](i:OID, s:ThingPtr, m:ThingPtr, val pType:PType[VT], val cType:Collection[VT, CT], pf:PropFetcher)
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
   * 
   * IMPORTANT: T must be exactly pType.valType. I still haven't figured out how
   * to deduce that automatically, though.
   * 
   * IMPORTANT: this is only legal on ExactlyOne Properties! It will throw
   * an exception otherwise.
   * 
   * TODO: change this to first(), and it should be legal everywhere.
   */
  def first(m:PropMap):VT = cType.get(from(m))

  // TODO: currently, this takes pType.valType as its input, and that's unchecked. This
  // is because I haven't figured out the correct syntax to get it to work correctly.
  // What I *want* is for it to instead take pType.rawType, and go through pType.apply()
  // to generate the correctly-typed ElemValue, and then feed that into cType.apply().
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