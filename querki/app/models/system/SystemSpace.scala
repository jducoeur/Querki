package models.system

import models._

/**
 * This is the master wrapper for the System Space. This is a hardcoded Space, living in
 * Shard 0. Note that all the OIDs are hardcoded, specifically so that they will be
 * stable. *NEVER* change an OID in this file!!!
 */
object SystemSpace {
  /**
   * The OID of the System Space itself. All these Things are contained in it.
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
        
    override def getProp(propId:ThingPtr):PropAndVal = {
      // If we've gotten up to here and haven't found the property, use
      // the default:
      val prop = space.prop(propId)
      localProp(propId).getOrElse(PropAndVal(prop, prop.default))
    }
    
    override def hasProp(propId:ThingPtr):Boolean = {
      props.contains(propId)
    }
  }
  
  /**
   * The Type for integers
   */
  object IntType extends PType(OID(0, 2), systemOID, UrThing) {
    type valType = Int
    
    override val props = toProps(
        name("Type-Whole-Number")
        )
    
    def deserialize(v:PropValue) = java.lang.Integer.parseInt(v.serialized)
    def serialize(v:valType) = PropValue(v.toString)
    def render(v:PropValue) = v.serialized
    
    val default = PropValue("0")
  }
  
  type Wikitext = String
  /**
   * The Type for Text -- probably the most common type in Querki
   */
  object TextType extends PType(OID(0, 3), systemOID, UrThing) {
    type valType = Wikitext
    
    override val props = toProps(
        name("Type-Text")
        )
        
    def apply(str:String) = Wikitext(str)
    
    def deserialize(v:PropValue) = v.serialized
    def serialize(v:valType) = PropValue(v)
    def render(v:PropValue) = v.serialized
    
    val default = PropValue("")
  }
  object Wikitext {
    def apply(str:String) = new Wikitext(str)
  }
  
  /**
   * The YesNo Type -- or Boolean, as us geeks think of it
   */
  object YesNoType extends PType(OID(0, 4), systemOID, UrThing) {
    type valType = Boolean
    
    override val props = toProps(
        name("Type-YesNo")
        )
    
    def deserialize(v:PropValue) = java.lang.Boolean.parseBoolean(v.serialized)
    def serialize(v:valType) = PropValue(v.toString)
    def render(v:PropValue) = v.serialized
    
    val default = PropValue("false")
  }
  
  // TODO: still need to add Collections!!!
  
  /**
   * The root Property, from which all others derive.
   */
  object UrProp extends Property(OID(0, 5), systemOID, UrThing, TextType) {
    override val props = toProps(
        name("Property")
        )
  }
  
  object NameProp extends Property(OID(0, 6), systemOID, UrProp, TextType) {
    override val props = toProps(
        name("Name")
        )
  }
  
  object DisplayTextProp extends Property(OID(0, 7), systemOID, UrProp, TextType) {
    override val props = toProps(
        name("Display-Text")
        )
  }
  
  object Page extends ThingState(OID(0, 8), systemOID, RootOID) {
    override val props = toProps(
        name("Simple-Page"),
        (DisplayTextProp -> PropValue("""
This is the basic Page Thing. Use it as your Model for *basic* Pages without real structure.
"""))
        )
  }
  
  val SystemUserOID = OID(0, 9)
  
  def oidMap[T <: Thing](items:T*):Map[OID,T] = {
    (Map.empty[OID,T] /: items) ((m, i) => m + (i.id -> i))
  }
  
  val types = oidMap[PType](IntType, TextType, YesNoType)
  val props = oidMap[Property](UrProp, NameProp, DisplayTextProp)
  val things = oidMap[ThingState](UrThing, Page)
  
  val State = SpaceState(systemOID, UrThing, SystemUserOID, "System", types, props, things)
}