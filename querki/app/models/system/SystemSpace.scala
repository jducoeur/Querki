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
  object UrProp extends Property(OID(0, 5), systemOID, UrThing, TextType, ExactlyOne) {
    override val props = toProps(
        setName("Property")
        )
  }
  
  val NameOID = OID(0, 6)
  object NameProp extends Property(NameOID, systemOID, UrProp, NameType, ExactlyOne) {
    override val props = toProps(
        setName("Name")
        )
  }
  
  object DisplayTextProp extends Property(OID(0, 7), systemOID, UrProp, TextType, Optional) {
    override val props = toProps(
        setName("Display-Text")
        )
  }
  
  object Page extends ThingState(OID(0, 8), systemOID, RootOID) {
    override val props = toProps(
        setName("Simple-Page"),
        (DisplayTextProp -> PropValue(Some(ElemValue(Wikitext("""
This is the basic Page Thing. Use it as your Model for *basic* Pages without real structure.
            
Use the **DisplayText** property to indicate what to show on the page. You can put anything in there.
""")))))
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
  
  //////////////////////////////////////
  //
  // Collections
  //
  
  // TODO: the Collections are full of fugly isInstanceOf expressions. I suspect that I
  // need fancy type math to get rid of these. Figure out how to make it work. It likely
  // has everything to do with the type signature of PropValue and ElemValue.
  
  /**
   * Root Collection type. Exists solely so that there is a common runtime root, in case
   * we want to be able to write new collections.
   */
  object UrCollection extends Collection(OID(0, 12), systemOID, UrThing) {
	type implType = Object
  
    def deserialize(ser:String, elemT:PType):PropValue[implType] = 
      throw new Error("Trying to deserialize root collection!")
    def serialize(v:PropValue[implType], elemT:PType):String = 
      throw new Error("Trying to serialize root collection!")
    def render(ser:PropValue[implType], elemT:PType):Wikitext = 
      throw new Error("Trying to render root collection!")
    def default(elemT:PType):PropValue[implType] = 
      throw new Error("Trying to default root collection!")    
  }
  
  case class OneColl[T <: ElemValue[_]](v:T)
  
  object ExactlyOne extends Collection(OID(0, 13), systemOID, UrCollection) {
    type implType = OneColl[_]
    
    def deserialize(ser:String, elemT:PType):PropValue[implType] = {
      PropValue(OneColl(elemT.deserialize(ser)))
    }
    def serialize(v:PropValue[implType], elemT:PType):String = {
      elemT.serialize(v.v.v.asInstanceOf[ElemValue[elemT.valType]])
    }
    def render(v:PropValue[implType], elemT:PType):Wikitext = {
      elemT.render(v.v.v.asInstanceOf[ElemValue[elemT.valType]])
    }
    def default(elemT:PType):PropValue[implType] = {
      PropValue(OneColl(elemT.default))
    }
  }
  
  object Optional extends Collection(OID(0, 14), systemOID, UrCollection) {
    type implType = Option[_]
    
    def deserialize(ser:String, elemT:PType):PropValue[implType] = {
      ser match {
        case "!" => PropValue(None)
        case s:String => {
          val elemStr = s.slice(1, s.length() - 1)
          PropValue(Some(elemT.deserialize(ser)))
        }
      }
    }
    
    def serialize(v:PropValue[implType], elemT:PType):String = {
      v.v match {
        case Some(elem) => "(" + elemT.serialize(v.asInstanceOf[ElemValue[elemT.valType]]) + ")"
        case None => "!"
      }
    }
    
    def render(v:PropValue[implType], elemT:PType):Wikitext = {
      v.v match {
        case Some(elem) => elemT.render(elem.asInstanceOf[ElemValue[elemT.valType]])
        case None => Wikitext("")
      }
    }
    
    def default(elemT:PType):PropValue[implType] = {
      PropValue(None)
    }
  }
  
  
  object QList extends Collection(OID(0, 15), systemOID, UrCollection) {
    type implType = List[_]
    
    def deserialize(ser:String, elemT:PType):PropValue[implType] = {
      val guts = ser.slice(1, ser.length() - 1)
      val elemStrs = guts.split(",").toList
      val elems = elemStrs.map(elemT.deserialize(_))
      PropValue(elems)
    }
    
    def serialize(v:PropValue[implType], elemT:PType):String = {
      v.v.
        map(elem => elemT.serialize(elem.asInstanceOf[ElemValue[elemT.valType]])).
        mkString("[", "," ,"]")
    }
    
    def render(v:PropValue[implType], elemT:PType):Wikitext = {
      val renderedElems = v.v.
        map(elem => elemT.render(elem.asInstanceOf[ElemValue[elemT.valType]]))
      Wikitext(renderedElems.mkString("\n"))
    }
    
    def default(elemT:PType):PropValue[implType] = {
      PropValue(List.empty)
    }
  }
  
  ///////////////////////////////
  
  def oidMap[T <: Thing](items:T*):Map[OID,T] = {
    (Map.empty[OID,T] /: items) ((m, i) => m + (i.id -> i))
  }
  
  val types = oidMap[PType](IntType, TextType, YesNoType, NameType)
  val props = oidMap[Property](UrProp, NameProp, DisplayTextProp)
  val things = oidMap[ThingState](UrThing, Page)
  
  object State extends SpaceState(systemOID, UrThing, SystemUserOID, "System", types, props, things) {
    override val props = toProps(
        setName("System"),
        (DisplayTextProp -> PropValue(Some(ElemValue(Wikitext("""
This is the fundamental System Space. Everything else derives from it.
""")))))
        )
  }
}