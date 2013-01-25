package models.system

import models._

import Thing._

import OIDs._

import ql._
  
//////////////////////////////////////
//
// Collections
//

abstract class SystemCollection(cid:OID, pf:PropFetcher) extends Collection(cid, systemOID, UrCollectionOID, pf)
  
  /**
   * Root Collection type. Exists solely so that there is a common runtime root, in case
   * we want to be able to write new collections.
   */
  class UrCollection extends Collection(UrCollectionOID, systemOID, UrThing,
      toProps(
        setName("Collection")
        )) 
  {
	type implType = List[ElemValue]
	
    def doDeserialize(ser:String, elemT:pType):implType = 
      throw new Error("Trying to deserialize root collection!")
    def doSerialize(v:implType, elemT:pType):String = 
      throw new Error("Trying to serialize root collection!")
    def doRender(context:ContextBase)(ser:implType, elemT:pType):Wikitext = 
      throw new Error("Trying to render root collection!")
    def doDefault(elemT:pType):implType = 
      throw new Error("Trying to default root collection!")    
	def wrap(elem:ElemValue):implType =
	  throw new Error("Trying to wrap root collection!")    
	def makePropValue(cv:implType):PropValue =
	  throw new Error("Trying to makePropValue root collection!")    
  }
  object UrCollection extends UrCollection
  
  /**
   * ExactlyOne is essentially Some -- it is quite intentionally Optional without the choice of None.
   * 
   * Sadly, though, Option isn't actually an Iterable, and trying to use type views to do this
   * was making me nuts.
   * 
   * TODO: rewrite ExactlyOne and Optional to be based on an actual Iterable with the right semantics.
   */
  class ExactlyOne(cid:OID) extends SystemCollection(cid,
    ExactlyOneProps.fetchProps) 
  {
    type implType = List[ElemValue]

	def doDeserialize(ser:String, elemT:pType):implType = {
      List(elemT.deserialize(ser))
    }
    def doSerialize(v:implType, elemT:pType):String = {
      elemT.serialize(v.head)
    }
    def doRender(context:ContextBase)(v:implType, elemT:pType):Wikitext = {
      elemT.render(context)(v.head)
    }
    def doDefault(elemT:pType):implType = {
      List(elemT.default)
    }
    def wrap(elem:ElemValue):implType = List(elem)
    def makePropValue(cv:implType):PropValue = ExactlyOnePropValue(cv)
    
    private case class ExactlyOnePropValue(cv:implType) extends PropValue {
      type myCollection = ExactlyOne
      
      val coll = ExactlyOne.this
    }
  }
  object ExactlyOne extends ExactlyOne(ExactlyOneOID)
  object ExactlyOneProps {
    def fetchProps:() => PropMap = {
      Thing.toProps(
        setName("Exactly-One")
      )
    }    
  }
  
  class Optional(cid:OID) extends SystemCollection(cid,
      toProps(
        setName("Optional")
        )) 
  {
    type implType = List[ElemValue]
    
    def doDeserialize(ser:String, elemT:pType):implType = {
      ser match {
        case "!" => Nil
        case s:String => {
          val elemStr = s.slice(1, s.length() - 1)
          List(elemT.deserialize(elemStr))
        }
      }
    }
    
    def doSerialize(v:implType, elemT:pType):String = {
      v match {
        case List(elem) => "(" + elemT.serialize(elem) + ")"
        case Nil => "!"
      }
    }
    
    def doRender(context:ContextBase)(v:implType, elemT:pType):Wikitext = {
      v match {
        case List(elem) => elemT.render(context)(elem)
        case Nil => Wikitext("")
      }
    }
    
    def doDefault(elemT:pType):implType = Nil
    
    def wrap(elem:ElemValue):implType = List(elem)
    def makePropValue(cv:implType):PropValue = OptionalPropValue(cv)
    
    val None:PropValue = makePropValue(Nil)
    
    private case class OptionalPropValue(cv:implType) extends PropValue {
      type myCollection = Optional
      
      val coll = Optional.this
    }
  }
  object Optional extends Optional(OptionalOID)
  
  
  class QList(cid:OID) extends SystemCollection(cid,
      toProps(
        setName("List")
        )) 
  {
    type implType = List[ElemValue]
    
    def doDeserialize(ser:String, elemT:pType):implType = {
      val guts = ser.slice(1, ser.length() - 1)
      val elemStrs = guts.split(",").toList
      elemStrs.map(elemT.deserialize(_))
    }
    
    def doSerialize(v:implType, elemT:pType):String = {
      v.map(elem => elemT.serialize(elem)).
        mkString("[", "," ,"]")
    }
    
    def doRender(context:ContextBase)(v:implType, elemT:pType):Wikitext = {
      val renderedElems = v.map(elem => elemT.render(context)(elem))
      Wikitext(renderedElems map (_.internal) mkString("\n"))
    }
    
    def doDefault(elemT:pType):implType = List.empty
    
    def wrap(elem:ElemValue):implType = List(elem)
    def makePropValue(cv:implType):PropValue = QListPropValue(cv)
    
    private case class QListPropValue(cv:implType) extends PropValue {
      type myCollection = QList
      
      val coll = QList.this
    }
  }
  object QList extends QList(QListOID)
  
object SystemCollections {
  def all = Space.oidMap[Collection](UrCollection, ExactlyOne, Optional, QList)
}