package models

import Thing._

/**
 * The value of a primitive Type. These are always considered "elements", since they
 * are always wrapped inside Collections.
 */
case class ElemValue[T](v:T)

case class PropValue[M](v:M)

/**
 * Properties have Types. There's nothing controversial here -- Types are usually
 * things like Text, Number and so on. But note that Types are themselves Things;
 * this is specifically so that we can potentially add user-defined Types down
 * the road.
 */
abstract class PType(i:OID, s:ThingPtr, m:ThingPtr, pf:PropFetcher) extends Thing(i, s, m, Kind.Type, pf) {
  
  type valType
  
  /**
   * Each PType is required to implement this -- it is the deserializer for the
   * type.
   */
  def deserialize(ser:String):ElemValue[valType]
  
  /**
   * Also required for all PTypes, to serialize values of this type.
   */
  def serialize(v:ElemValue[valType]):String
  
  /**
   * Takes a value of this type, and turns it into displayable form. Querki
   * equivalent to toString.
   */
  def render(ser:ElemValue[valType]):Wikitext
  
  /**
   * Also required for all PTypes -- the default value to fall back on.
   */
  def default:ElemValue[valType]
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
abstract class Collection(i:OID, s:ThingPtr, m:ThingPtr, pf:PropFetcher) extends Thing(i, s, m, Kind.Collection, pf) {
  type implType
  
  /**
   * Each Collection is required to implement this -- it is the deserializer for the
   * type.
   */
  def deserialize(ser:String, elemT:PType):PropValue[implType]
  
  /**
   * Also required for all Collections, to serialize values of this type.
   */
  def serialize(v:PropValue[implType], elemT:PType):String
  
  /**
   * Takes a value of this type, and turns it into displayable form. Querki
   * equivalent to toString.
   */
  def render(v:PropValue[implType], elemT:PType):Wikitext
  
  /**
   * Also required for all Collections -- the default value to fall back on.
   */
  def default(elemT:PType):PropValue[implType]
}

/**
 * A Property is a field that may exist on a Thing. It is, itself, a Thing with a
 * specific Type.
 */
case class Property(i:OID, s:ThingPtr, m:ThingPtr, val pType:PType, val cType:Collection, pf:PropFetcher)
  extends Thing(i, s, m, Kind.Property, pf) {
  def default = {
    // TODO: add the concept of the default meta-property, so you can set it
    // on a prop-by-prop basis
    cType.default(pType)
  }
  
  def render(v:PropValue[cType.implType]) = cType.render(v, pType)
}

/**
 * A convenient wrapper for passing a value around in a way that can be fetched.
 */
case class PropAndVal(prop:Property, v:PropValue[_]) {
  type valType = PropValue[prop.cType.implType]
  
  def get = v.v
  def render = prop.render(v.asInstanceOf[valType])
}