package models.system

import models._
import models.ThingPtr._

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
   * The Ur-Thing, from which the entire world descends. Note that this is
   * its own parent!
   */
  object UrThing extends ThingState(RootOID, systemOID, RootOID) {
    override val props = toProps(
        name("Thing")
        )
  }
  
  /**
   * The Type for integers
   */
  object IntType extends PType(OID(0, 2), systemOID, RootOID) {
    class IntVal(val num:Int) extends PropValue
    type valType = IntVal
    
    override val props = toProps(
        name("Type-Whole-Number")
        )
    
    def deserialize(str:String) = new IntVal(java.lang.Integer.parseInt(str))
    def serialize(v:valType) = v.toString()
  }
  
  val TextOID = OID(0, 3)
  /**
   * The Type for Text -- probably the most common type in Querki
   */
  object TextType extends PType(TextOID, systemOID, RootOID) {
    class TextVal(val text:String) extends PropValue
    type valType = TextVal
    
    override val props = toProps(
        name("Type-Text")
        )
    
    def deserialize(str:String) = new TextVal(str)
    def apply(str:String) = deserialize(str)
    def serialize(v:valType) = v.text
  }
  
  /**
   * The YesNo Type -- or Boolean, as us geeks think of it
   */
  object YesNoType extends PType(OID(0, 4), systemOID, RootOID) {
    class YesNoVal(val b:Boolean) extends PropValue
    type valType = YesNoVal
    
    override val props = toProps(
        name("Type-YesNo")
        )
    
    def deserialize(str:String) = new YesNoVal(java.lang.Boolean.parseBoolean(str))
    def serialize(v:valType) = v.b.toString
  }
  
  val PropOID = OID(0, 5)
  /**
   * The root Property, from which all others derive.
   */
  object UrProp extends Property(PropOID, systemOID, RootOID, TextType) {
    override val props = toProps(
        name("Property")
        )
  }
  
  val NameOID = OID(0, 6)
  object NameProp extends Property(NameOID, systemOID, PropOID, TextType) {
    override val props = toProps(
        name("Name")
        )
  }
  
//  object Page extends ThingState(, systemOID, RootOID) {
//    override val props = Map[OID, PropValue](
//        )
//  }
}