package models

import models.system.SystemSpace.Wikitext

/**
 * The value of a Property on a Thing. This is kept as a String, and evaluated
 * on-the-fly as necessary.
 */
case class PropValue(serialized:String)

/**
 * Properties have Types. There's nothing controversial here -- Types are usually
 * things like Text, Number and so on. But note that Types are themselves Things;
 * this is specifically so that we can potentially add user-defined Types down
 * the road.
 */
abstract class PType(i:OID, s:ThingPtr, m:ThingPtr) extends Thing(i, s, m, Kind.Type) {
  
  type valType
  
  /**
   * Each PType is required to implement this -- it is the deserializer for the
   * type.
   */
  def deserialize(ser:PropValue):valType
  
  /**
   * Also required for all PTypes, to serialize values of this type.
   */
  def serialize(v:valType):PropValue
  
  /**
   * Takes a value of this type, and turns it into displayable form. Querki
   * equivalent to toString.
   */
  def render(ser:PropValue):Wikitext
  
  /**
   * Also required for all PTypes -- the default value to fall back on.
   */
  def default:PropValue
}

/**
 * A Property is a field that may exist on a Thing. It is, itself, a Thing with a
 * specific Type.
 */
case class Property(i:OID, s:ThingPtr, m:ThingPtr, val pType:PType) extends Thing(i, s, m, Kind.Property) {
  def default = {
    // TODO: this should route through the Collection type instead
    // TODO: add the concept of the default meta-property, so you can set it
    // on a prop-by-prop basis
    pType.default
  }
}

/**
 * A convenient wrapper for passing a value around in a way that can be fetched.
 */
case class PropAndVal(prop:Property, v:PropValue) {
  type valType = prop.pType.valType
  
  def get = prop.pType.deserialize(v)
}