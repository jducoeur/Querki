package models.system

import play.api._

import models._

import Thing._

import OID.thing2OID

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
    override def getProp(propId:OID):PropAndVal[_,_] = {
      // If we've gotten up to here and haven't found the property, use
      // the default:
      val prop = space.prop(propId)
      localProp(propId).getOrElse(prop.defaultPair)
    }
    
    override def hasProp(propId:OID):Boolean = {
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
    def doDeserialize(v:String) = java.lang.Integer.parseInt(v)
    def doSerialize(v:Int) = v.toString
    def doRender(v:Int) = Wikitext(v.toString)
    
    val doDefault = 0
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
    
    def doDeserialize(v:String) = Wikitext(v)
    def doSerialize(v:Wikitext) = v.internal
    def doRender(v:Wikitext) = v
    
    val doDefault = Wikitext("")
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
    def doDeserialize(ser:String) = java.lang.Boolean.parseBoolean(ser)
    def doSerialize(v:Boolean) = v.toString
    def doRender(v:Boolean) = Wikitext(v.toString())
    
    val doDefault = false
  }

  /**
   * The root Property, from which all others derive.
   */
  class UrProp extends Property(OID(0, 5), systemOID, UrThing, TextType, ExactlyOne,
      toProps(
        setName("Property")
        ))
  object UrProp extends UrProp
  
  val NameOID = OID(0, 6)
  object NameProp extends Property(NameOID, systemOID, UrProp, NameType, ExactlyOne,
      toProps(
        setName("Name")
        ))
  
  object DisplayTextProp extends Property(OID(0, 7), systemOID, UrProp, TextType, Optional,
      toProps(
        setName("Display-Text")
        ))
  
  object Page extends ThingState(OID(0, 8), systemOID, RootOID,
      toProps(
        setName("Simple-Page"),
        DisplayTextProp("""
This is the basic Page Thing. Use it as your Model for *basic* Pages without real structure.
            
Use the **DisplayText** property to indicate what to show on the page. You can put anything in there.
""")))
  
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
        
    def doDeserialize(v:String) = toDisplay(v)
    def doSerialize(v:String) = toInternal(v)
    def doRender(v:String) = Wikitext(toDisplay(v))
    
    val doDefault = "MISSING NAME!"
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
  class UrCollection extends Collection[Option[ElemValue]](OID(0, 12), systemOID, UrThing,
      toProps(
        setName("Collection")
        )) 
  {
    def doDeserialize(ser:String, elemT:pType):implType = 
      throw new Error("Trying to deserialize root collection!")
    def doSerialize(v:implType, elemT:pType):String = 
      throw new Error("Trying to serialize root collection!")
    def doRender(ser:implType, elemT:pType):Wikitext = 
      throw new Error("Trying to render root collection!")
    def doDefault(elemT:pType):implType = 
      throw new Error("Trying to default root collection!")    
	def wrap(elem:ElemValue):implType =
	  throw new Error("Trying to wrap root collection!")    
    def doFirst(pv:implType):ElemValue =
      throw new Error("Trying to wrap root collection!")
  }
  object UrCollection extends UrCollection
  
  case class OneColl(v:ElemValue)
  
  class ExactlyOne extends Collection[OneColl](
    OID(0, 13), 
    systemOID, 
    UrCollection,
    toProps(
      setName("Exactly-One")
      )) 
  {
	def doDeserialize(ser:String, elemT:pType):implType = {
      OneColl(elemT.deserialize(ser))
    }
    def doSerialize(v:implType, elemT:pType):String = {
      elemT.serialize(v.v)
    }
    def doRender(v:implType, elemT:pType):Wikitext = {
      elemT.render(v.v)
    }
    def doDefault(elemT:pType):implType = {
      OneColl(elemT.default)
    }
    def wrap(elem:ElemValue):implType = OneColl(elem)
    
    def doFirst(v:implType):ElemValue = v.v
  }
  object ExactlyOne extends ExactlyOne
  
  class Optional extends Collection[Option[ElemValue]](OID(0, 14), systemOID, UrCollection,
      toProps(
        setName("Optional")
        )) 
  {
    def doDeserialize(ser:String, elemT:pType):implType = {
      ser match {
        case "!" => None
        case s:String => {
          val elemStr = s.slice(1, s.length() - 1)
          Some(elemT.deserialize(elemStr))
        }
      }
    }
    
    def doSerialize(v:implType, elemT:pType):String = {
      v match {
        case Some(elem) => "(" + elemT.serialize(elem) + ")"
        case None => "!"
      }
    }
    
    def doRender(v:implType, elemT:pType):Wikitext = {
      v match {
        case Some(elem) => elemT.render(elem)
        case None => Wikitext("")
      }
    }
    
    def doDefault(elemT:pType):implType = None
    
    def wrap(elem:ElemValue):implType = Some(elem)
    
    def doFirst(pv:implType):ElemValue = pv match {
      case Some(elemV) => elemV
      case None => throw new Exception("Trying to first on an empty Optional!")
    }
  }
  object Optional extends Optional
  
  
  class QList extends Collection[List[ElemValue]](OID(0, 15), systemOID, UrCollection,
      toProps(
        setName("List")
        )) 
  {
    def doDeserialize(ser:String, elemT:pType):implType = {
      val guts = ser.slice(1, ser.length() - 1)
      val elemStrs = guts.split(",").toList
      elemStrs.map(elemT.deserialize(_))
    }
    
    def doSerialize(v:implType, elemT:pType):String = {
      v.map(elem => elemT.serialize(elem)).
        mkString("[", "," ,"]")
    }
    
    def doRender(v:implType, elemT:pType):Wikitext = {
      val renderedElems = v.map(elem => elemT.render(elem))
      Wikitext(renderedElems.mkString("\n"))
    }
    
    def doDefault(elemT:pType):implType = List.empty
    
    def wrap(elem:ElemValue):implType = List(elem)
    
    def doFirst(v:implType):ElemValue = v.head
  }
  object QList extends QList
  
  ///////////////////////////////
  
  /**
   * The Type for Links to other Things
   */
  object LinkType extends PType[OID](OID(0, 16), systemOID, UrThing,
      toProps(
        setName("Type-Link")
        )) with SimplePTypeBuilder[OID]
  {
    def doDeserialize(v:String) = OID(v)
    def doSerialize(v:OID) = v.toString
    // TODO: this is a good illustration of the fact that render should actually
    // be contextual -- you can't really render a Link in isolation, without knowing
    // about the Thing it points to:
    def doRender(v:OID) = Wikitext(v.toString)

    val doDefault = UnknownOID
  }
    
  /**
   * The Property that points from a Property to its Type.
   */
  object TypeProp extends Property(OID(0, 17), systemOID, UrProp, LinkType, ExactlyOne,
      toProps(
        setName("__Type")
        ))
  
  /**
   * The Property that points from a Property to its Collection.
   */
  object CollectionProp extends Property(OID(0, 18), systemOID, UrProp, LinkType, ExactlyOne,
      toProps(
        setName("__Collection")
        ))
  
  ///////////////////////////////
  
  def oidMap[T <: Thing](items:T*):Map[OID,T] = {
    (Map.empty[OID,T] /: items) ((m, i) => m + (i.id -> i))
  }
  
  val types = oidMap[PType[_]](IntType, TextType, YesNoType, NameType, LinkType)
  val props = oidMap[Property[_,_,_]](UrProp, NameProp, DisplayTextProp, TypeProp, CollectionProp)
  val things = oidMap[ThingState](UrThing, Page)
  val colls = oidMap[Collection[_]](UrCollection, ExactlyOne, Optional, QList)
  
  object State extends SpaceState(systemOID, UrThing,
      toProps(
        setName("System"),
        DisplayTextProp("""
This is the fundamental System Space. Everything else derives from it.
""")
        ), SystemUserOID, "System", None, types, props, things, colls)
}