package models

/**
 * Base class for values that exist in Querki Things. PTypes act as factories for
 * PropValues.
 */
abstract class PropValue() {}

/**
 * Properties have Types. There's nothing controversial here -- Types are usually
 * things like Text, Number and so on. But note that Types are themselves Things;
 * this is specifically so that we can potentially add user-defined Types down
 * the road.
 */
abstract class PType(i:OID, s:OID, m:OID) extends Thing(i, s, m, Kind.Type) {
  
  type valType <: PropValue
  
  /**
   * Each PType is required to implement this -- it is the deserializer for the
   * type.
   */
  def deserialize(str:String):valType
  
  /**
   * Also required for all PTypes, to serialize values of this type.
   */
  def serialize(v:valType):String
}

/**
 * A Property is a field that may exist on a Thing. It is, itself, a Thing with a
 * specific Type.
 */
case class Property(i:OID, s:OID, m:OID, val pType:PType) extends Thing(i, s, m, Kind.Property) {
  
}
