package models.system

import play.api._

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
        setName("Thing")
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
        setName("Type-Whole-Number")
        )
    
    def deserialize(v:String) = ElemValue(java.lang.Integer.parseInt(v))
    def serialize(v:ElemValue[valType]) = v.v.toString
    def render(v:ElemValue[valType]) = Wikitext(v.v.toString())
    
    val default = ElemValue(0)
  }
  
  /**
   * The Type for Text -- probably the most common type in Querki
   */
  object TextType extends PType(OID(0, 3), systemOID, UrThing) {
    type valType = Wikitext
    
    override val props = toProps(
        setName("Type-Text")
        )
        
    def apply(str:String) = Wikitext(str)
    
    def deserialize(v:String) = ElemValue(Wikitext(v))
    def serialize(v:ElemValue[valType]) = v.v.internal
    def render(v:ElemValue[valType]) = v.v
    
    val default = ElemValue(Wikitext(""))
  }
  
  /**
   * The YesNo Type -- or Boolean, as us geeks think of it
   */
  object YesNoType extends PType(OID(0, 4), systemOID, UrThing) {
    type valType = Boolean
    
    override val props = toProps(
        setName("Type-YesNo")
        )
    
    def deserialize(ser:String) = ElemValue(java.lang.Boolean.parseBoolean(ser))
    def serialize(v:ElemValue[valType]) = v.v.toString
    def render(v:ElemValue[valType]) = Wikitext(v.v.toString())
    
    val default = ElemValue(false)
  }
  
  // TODO: still need to add Collections!!!
  
  /**
   * The root Property, from which all others derive.
   */
  object UrProp extends Property(OID(0, 5), systemOID, UrThing, TextType) {
    override val props = toProps(
        setName("Property")
        )
  }
  
  val NameOID = OID(0, 6)
  object NameProp extends Property(NameOID, systemOID, UrProp, NameType) {
    override val props = toProps(
        setName("Name")
        )
  }
  
  object DisplayTextProp extends Property(OID(0, 7), systemOID, UrProp, TextType) {
    override val props = toProps(
        setName("Display-Text")
        )
  }
  
  object Page extends ThingState(OID(0, 8), systemOID, RootOID) {
    override val props = toProps(
        setName("Simple-Page"),
        (DisplayTextProp -> ElemValue(Wikitext("""
This is the basic Page Thing. Use it as your Model for *basic* Pages without real structure.
            
Use the **DisplayText** property to indicate what to show on the page. You can put anything in there.
""")))
        )
  }
  
  val SystemUserOID = OID(0, 9)
  
  /**
   * The Type for Display Names -- similar to Text, but not identical
   */
  object NameType extends PType(OID(0, 10), systemOID, UrThing) {
    type valType = String
    
    override val props = toProps(
        setName("Type-Name")
        )

    def toInternal(str:String) = str.replaceAll(" ", "-")
    def toDisplay(str:String) = str.replaceAll("-", " ")
        
    def apply(str:String) = toInternal(str)
    
    def deserialize(v:String) = ElemValue(toDisplay(v))
    def serialize(v:ElemValue[valType]) = toInternal(v.v)
    def render(v:ElemValue[valType]) = Wikitext(toDisplay(v.v))
    
    val default = ElemValue("MISSING NAME!")
  }
  
  val TestUserOID = OID(0, 11)
//  
//  /**
//   * Root Collection type. Exists solely so that there is a common runtime root, in case
//   * we want to be able to write new collections.
//   */
//  object UrCollection extends Collection(OID(0, 12), systemOID, UrThing) {
//	type implType = Object
//  
//    def deserialize(ser:PropValue, elemT:PType):implType = 
//      throw new Error("Trying to deserialize root collection!")
//    def serialize(v:implType, elemT:PType):PropValue = 
//      throw new Error("Trying to serialize root collection!")
//    def render(ser:PropValue, elemT:PType):Wikitext = 
//      throw new Error("Trying to render root collection!")
//    def default(elemT:PType):PropValue = 
//      throw new Error("Trying to default root collection!")    
//  }
//  
//  object ExactlyOne extends Collection(OID(0, 13), systemOID, UrCollection) {
//    type implType = Any
//    
//    def deserialize(ser:PropValue, elemT:PType):implType = {
//      elemT.deserialize(ser)
//    }
//    def serialize(v:implType, elemT:PType):PropValue = {
//      elemT.serialize(v.asInstanceOf[elemT.valType])
//    }
//    def render(ser:PropValue, elemT:PType):Wikitext = {
//      elemT.render(ser)
//    }
//    def default(elemT:PType):PropValue = {
//      elemT.default
//    }
//  }
//  
//  object Optional extends Collection(OID(0, 14), systemOID, UrCollection) {
//    type implType = Option[_]
//    
//    def deserialize(ser:PropValue, elemT:PType):implType = {
//      ser.serialized match {
//        case "!" => None
//        case s:String => {
//          val elemStr = s.slice(1, s.length() - 1)
//          Some(elemT.deserialize(ser))
//        }
//      }
//    }
//    
//    def serialize(v:implType, elemT:PType):PropValue = {
//      v match {
//        case Some(elem) => PropValue("(" + elemT.serialize(v.asInstanceOf[elemT.valType]) + ")")
//        case None => PropValue ("!")
//      }
//    }
//    
//    def render(ser:PropValue, elemT:PType):Wikitext = {
//      val v = deserialize(ser, elemT)
//      v match {
//        case Some(elem) => elemT.render(elem)
//        case None => Wikitext("")
//      }
//    }
//    
//    def default(elemT:PType):PropValue = {
//      elemT.default
//    }
//  }
  
  def oidMap[T <: Thing](items:T*):Map[OID,T] = {
    (Map.empty[OID,T] /: items) ((m, i) => m + (i.id -> i))
  }
  
  val types = oidMap[PType](IntType, TextType, YesNoType, NameType)
  val props = oidMap[Property](UrProp, NameProp, DisplayTextProp)
  val things = oidMap[ThingState](UrThing, Page)
  
  object State extends SpaceState(systemOID, UrThing, SystemUserOID, "System", types, props, things) {
    override val props = toProps(
        setName("System"),
        (DisplayTextProp -> ElemValue(Wikitext("""
This is the fundamental System Space. Everything else derives from it.
""")))
        )
  }
}