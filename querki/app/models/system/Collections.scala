package models.system

import models._

import Thing._

import OIDs._
  
//////////////////////////////////////
//
// Collections
//

abstract class SystemCollection[T](cid:OID, pf:PropFetcher) extends Collection[T](cid, systemOID, UrCollectionOID, pf)
  
  /**
   * Root Collection type. Exists solely so that there is a common runtime root, in case
   * we want to be able to write new collections.
   */
  class UrCollection extends Collection[Option[ElemValue]](UrCollectionOID, systemOID, UrThing,
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
    def doIsEmpty(pv:implType) =
      throw new Error("Trying to isEmpty root collection!")
  }
  object UrCollection extends UrCollection
  
  case class OneColl(v:ElemValue)
  
  class ExactlyOne(cid:OID) extends SystemCollection[OneColl](cid,
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
    
    def doIsEmpty(v:implType) = false
  }
  object ExactlyOne extends ExactlyOne(ExactlyOneOID)
  
  class Optional(cid:OID) extends SystemCollection[Option[ElemValue]](cid,
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
    
    def doIsEmpty(v:implType) = v.isEmpty
  }
  object Optional extends Optional(OptionalOID)
  
  
  class QList(cid:OID) extends SystemCollection[List[ElemValue]](cid,
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
    
    def doIsEmpty(v:implType) = v.isEmpty
  }
  object QList extends QList(QListOID)
  