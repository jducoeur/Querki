package models.system

import play.api._

import models._

import Thing._

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
  object UrThing extends ThingState(RootOID, systemOID, RootOID,
      toProps(
        setName("Thing")
        )) 
  {
    override def getProp(propId:ThingPtr):PropAndVal[_,_] = {
      // If we've gotten up to here and haven't found the property, use
      // the default:
      val prop = space.prop(propId)
      localProp(propId).getOrElse(prop.defaultPair)
    }
    
    override def hasProp(propId:ThingPtr):Boolean = {
      props.contains(propId)
    }
  }
  
  /**
   * The Type for integers
   */
  object IntType extends PType[Int](OID(0, 2), systemOID, UrThing,
      toProps(
        setName("Type-Whole-Number")
        )) with SimplePTypeBuilder[Int]
  {
    def deserialize(v:String) = ElemValue(java.lang.Integer.parseInt(v))
    def serialize(v:elemVT) = v.elem.toString
    def render(v:elemVT) = Wikitext(v.elem.toString())
    
    val default = ElemValue(0)
  }
  
  /**
   * The Type for Text -- probably the most common type in Querki
   */
  object TextType extends PType[Wikitext](OID(0, 3), systemOID, UrThing,
      toProps(
        setName("Type-Text")
        )) with PTypeBuilder[Wikitext,String]
  {
    // TODO: escape JSON special chars for serialization!
    
    def deserialize(v:String) = ElemValue(Wikitext(v))
    def serialize(v:elemVT) = v.elem.internal
    def render(v:elemVT) = v.elem
    
    val default = ElemValue(Wikitext(""))
    def wrap(raw:String):valType = Wikitext(raw)
  }
  
  /**
   * The YesNo Type -- or Boolean, as us geeks think of it
   */
  object YesNoType extends PType[Boolean](OID(0, 4), systemOID, UrThing,
      toProps(
        setName("Type-YesNo")
        )) with SimplePTypeBuilder[Boolean]
  {
    def deserialize(ser:String) = ElemValue(java.lang.Boolean.parseBoolean(ser))
    def serialize(v:elemVT) = v.elem.toString
    def render(v:elemVT) = Wikitext(v.elem.toString())
    
    val default = ElemValue(false)
  }

  /**
   * The root Property, from which all others derive.
   */
  object ExactlyOneText extends ExactlyOne[Wikitext]
  class UrProp extends Property(OID(0, 5), systemOID, UrThing, TextType, ExactlyOneText,
      toProps(
        setName("Property")
        ))
  object UrProp extends UrProp
  
  object ExactlyOneString extends ExactlyOne[String]
  val NameOID = OID(0, 6)
  object NameProp extends Property(NameOID, systemOID, UrProp, NameType, ExactlyOneString,
      toProps(
        setName("Name")
        ))
  
  object OptionalText extends Optional[Wikitext]
  object DisplayTextProp extends Property(OID(0, 7), systemOID, UrProp, TextType, OptionalText,
      toProps(
        setName("Display-Text")
        ))
  
  object Page extends ThingState(OID(0, 8), systemOID, RootOID,
      toProps(
        setName("Simple-Page"),
        DisplayTextProp(Wikitext("""
This is the basic Page Thing. Use it as your Model for *basic* Pages without real structure.
            
Use the **DisplayText** property to indicate what to show on the page. You can put anything in there.
"""))))
  
  val SystemUserOID = OID(0, 9)
  
  /**
   * The Type for Display Names -- similar to Text, but not identical
   * 
   * TODO: introduce validation, since only a subset of chars are legal (I think)
   */
  class NameType extends PType[String](OID(0, 10), systemOID, UrThing,
      toProps(
        setName("Type-Name")
        )) with SimplePTypeBuilder[String]
  {
    def toInternal(str:String) = str.replaceAll(" ", "-")
    def toDisplay(str:String) = str.replaceAll("-", " ")
        
    def deserialize(v:String) = ElemValue(toDisplay(v))
    def serialize(v:elemVT) = toInternal(v.elem)
    def render(v:elemVT) = Wikitext(toDisplay(v.elem))
    
    val default = ElemValue("MISSING NAME!")
  }
  object NameType extends NameType
  
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
  class UrCollection[VT] extends Collection[VT, Option[VT]](OID(0, 12), systemOID, UrThing,
      toProps(
        setName("Collection")
        )) 
  {
    def deserialize(ser:String, elemT:pType):PropValue[implType] = 
      throw new Error("Trying to deserialize root collection!")
    def serialize(v:PropValue[implType], elemT:pType):String = 
      throw new Error("Trying to serialize root collection!")
    def render(ser:PropValue[implType], elemT:pType):Wikitext = 
      throw new Error("Trying to render root collection!")
    def default(elemT:pType):PropValue[implType] = 
      throw new Error("Trying to default root collection!")    
	def wrap(elem:ElemValue[VT,VT]):implType =
	  throw new Error("Trying to wrap root collection!")    
  }
  object UrCollection extends UrCollection
  
  case class OneColl[VT](v:ElemValue[VT, VT])
  
  class ExactlyOne[VT] extends Collection[VT, OneColl[VT]](
    OID(0, 13), 
    systemOID, 
    UrCollection,
    toProps(
      setName("Exactly-One")
      )) 
  {
	def deserialize(ser:String, elemT:pType):PropValue[implType] = {
      PropValue(OneColl(elemT.deserialize(ser)))
    }
    def serialize(v:PropValue[implType], elemT:pType):String = {
      elemT.serialize(v.v.v.asInstanceOf[elemVT])
    }
    def render(v:PropValue[implType], elemT:pType):Wikitext = {
      elemT.render(v.v.v.asInstanceOf[elemVT])
    }
    def default(elemT:pType):PropValue[implType] = {
      PropValue(OneColl(elemT.default))
    }
    def wrap(elem:ElemValue[VT,VT]):implType = OneColl(elem)
    
//    override def get(pv:PropValue[implType]):VT = {
//      val coll:implType = pv.v
//      val elemV:elemVT = coll.v
//      elemV.elem
//    }
  }
  object ExactlyOne extends ExactlyOne
  
  class Optional[VT] extends Collection[VT, Option[ElemValue[VT, VT]]](OID(0, 14), systemOID, UrCollection,
      toProps(
        setName("Optional")
        )) 
  {
    def deserialize(ser:String, elemT:pType):PropValue[implType] = {
      ser match {
        case "!" => PropValue(None)
        case s:String => {
          val elemStr = s.slice(1, s.length() - 1)
          PropValue(Some(elemT.deserialize(ser)))
        }
      }
    }
    
    def serialize(v:PropValue[implType], elemT:pType):String = {
      v.v match {
        case Some(elem) => "(" + elemT.serialize(elem.asInstanceOf[elemVT]) + ")"
        case None => "!"
      }
    }
    
    def render(v:PropValue[implType], elemT:pType):Wikitext = {
      v.v match {
        case Some(elem) => elemT.render(elem.asInstanceOf[elemVT])
        case None => Wikitext("")
      }
    }
    
    def default(elemT:pType):PropValue[implType] = {
      PropValue(None)
    }
    
    def wrap(elem:elemVT):implType = Some(elem)
  }
  object Optional extends Optional
  
  
  class QList[VT] extends Collection[VT, List[ElemValue[VT, VT]]](OID(0, 15), systemOID, UrCollection,
      toProps(
        setName("List")
        )) 
  {
    def deserialize(ser:String, elemT:pType):PropValue[implType] = {
      val guts = ser.slice(1, ser.length() - 1)
      val elemStrs = guts.split(",").toList
      val elems = elemStrs.map(elemT.deserialize(_))
      PropValue(elems)
    }
    
    def serialize(v:PropValue[implType], elemT:pType):String = {
      v.v.
        map(elem => elemT.serialize(elem.asInstanceOf[elemVT])).
        mkString("[", "," ,"]")
    }
    
    def render(v:PropValue[implType], elemT:pType):Wikitext = {
      val renderedElems = v.v.
        map(elem => elemT.render(elem.asInstanceOf[elemVT]))
      Wikitext(renderedElems.mkString("\n"))
    }
    
    def default(elemT:pType):PropValue[implType] = {
      PropValue(List.empty)
    }
    
    def wrap(elem:elemVT):implType = List(elem)
  }
  object QList extends QList
  
  ///////////////////////////////
  
  /**
   * The Type for Links to other Things
   */
  object LinkType extends PType[OID](OID(0, 16), systemOID, UrThing,
      toProps(
        setName("Type-Link")
        )) with PTypeBuilder[OID, ThingPtr]
  {
    def deserialize(v:String) = ElemValue(OID(v))
    def serialize(v:elemVT) = v.elem.toString
    // TODO: this is a good illustration of the fact that render should actually
    // be contextual -- you can't really render a Link in isolation, without knowing
    // about the Thing it points to:
    def render(v:elemVT) = Wikitext(v.elem.toString)

    val default = ElemValue[valType, valType](UnknownOID)
    def wrap(raw:ThingPtr):valType = raw.id
  }
    
  object ExactlyOneLink extends ExactlyOne[OID]
  /**
   * The Property that points from a Property to its Type.
   */
  object TypeProp extends Property(OID(0, 17), systemOID, UrProp, LinkType, ExactlyOneLink,
      toProps(
        setName("__Type")
        ))
  
  /**
   * The Property that points from a Property to its Collection.
   */
  object CollectionProp extends Property(OID(0, 18), systemOID, UrProp, LinkType, ExactlyOneLink,
      toProps(
        setName("__Collection")
        ))
  
  ///////////////////////////////
  
  def oidMap[T <: Thing](items:T*):Map[OID,T] = {
    (Map.empty[OID,T] /: items) ((m, i) => m + (i.id -> i))
  }
  
  val types = oidMap[PType[_]](IntType, TextType, YesNoType, NameType, LinkType)
  val props = oidMap[Property[_,_]](UrProp, NameProp, DisplayTextProp, TypeProp, CollectionProp)
  val things = oidMap[ThingState](UrThing, Page)
  
  object State extends SpaceState(systemOID, UrThing,
      toProps(
        setName("System"),
        DisplayTextProp(Wikitext("""
This is the fundamental System Space. Everything else derives from it.
"""))
        ), SystemUserOID, "System", None, types, props, things)
}