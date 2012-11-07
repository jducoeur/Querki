package models.system

import models._

/**
 * This is the master wrapper for the System Space. This is a hardcoded Space, living in
 * Shard 0. Note that all the OIDs are hardcoded, specifically so that they will be
 * stable. *NEVER* change an OID in this file!!!
 */
object SystemSpace {
  /**
   * The OID of the System Space
   */
  val systemOID = OID(0, 0)
  
  val RootOID = OID(0, 1)
  /**
   * The Ur-Thing, from which the entire world descends
   */
  val UrThing = ThingState(RootOID, systemOID, RootOID)
  
  /**
   * The Type for integers
   */
  object IntType extends PType(OID(0, 2), systemOID, RootOID) {
    class IntVal(val num:Int) extends PropValue
    type valType = IntVal
    
    def deserialize(str:String) = new IntVal(java.lang.Integer.parseInt(str))
    def serialize(v:valType) = v.toString()
  }
  
  /**
   * The Type for Text -- probably the most common type in Querki
   */
  object TextType extends PType(OID(0, 3), systemOID, RootOID) {
    class TextVal(val text:String) extends PropValue
    type valType = TextVal
    
    def deserialize(str:String) = new TextVal(str)
    def serialize(v:valType) = v.text
  }
  
  /**
   * The YesNo Type -- or Boolean, as us geeks think of it
   */
  object YesNoType extends PType(OID(0, 4), systemOID, RootOID) {
    class YesNoVal(val b:Boolean) extends PropValue
    type valType = YesNoVal
    
    def deserialize(str:String) = new YesNoVal(java.lang.Boolean.parseBoolean(str))
    def serialize(v:valType) = v.b.toString
  }
  
  object Page extends ThingState(OID(0, 5), systemOID, RootOID) {
    override val props = Map[OID, PropValue](
        )
  }
}