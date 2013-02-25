package models.system

import scala.xml._

import play.api.Logger

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
    def renderInput(prop:Property[_,_], state:SpaceState, currentValue:DisplayPropVal, elemT:PType[_]):scala.xml.Elem =
	  throw new Error("Trying to render input on root collection!")    
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
    
    def renderInput(prop:Property[_,_], state:SpaceState, currentValue:DisplayPropVal, elemT:PType[_]):scala.xml.Elem = {
      val v = currentValue.v.map(_.first).getOrElse(elemT.default)
      elemT.renderInput(prop, state, currentValue, v)
    }

    def makePropValue(cv:implType):PropValue = ExactlyOnePropValue(cv, this)
    private case class ExactlyOnePropValue(cv:implType, coll:ExactlyOne) extends PropValue
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
    def makePropValue(cv:implType):PropValue = OptionalPropValue(cv, this)    
    private case class OptionalPropValue(cv:implType, coll:Optional) extends PropValue
    
    def renderInput(prop:Property[_,_], state:SpaceState, currentValue:DisplayPropVal, elemT:PType[_]):scala.xml.Elem = {
      // TODO: what should we do here? Has custom rendering become unnecessary here? Does the appearance of the
      // trash button eliminate the need for anything fancy for Optional properties?
      val v = currentValue.v.map(_.first).getOrElse(elemT.default)
      elemT.renderInput(prop, state, currentValue, v)
    }

    val None:PropValue = makePropValue(Nil)
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
    def makePropValue(cv:implType):PropValue = QListPropValue(cv, this)
    private case class QListPropValue(cv:implType, coll:QList) extends PropValue
    
    // TODO: the stuff created here overlaps badly with the Javascript code in editThing.scala.html.
    // Rationalize the two, to eliminate all the duplication. In theory, the concept and structure
    // belongs here, and the details belong there.
    def renderInput(prop:Property[_,_], state:SpaceState, currentValue:DisplayPropVal, elemT:PType[_]):scala.xml.Elem = {
      val inputTemplate = elemT.renderInput(prop, state, currentValue, elemT.default) %
    		  Attribute("class", Text("inputTemplate"),
    		  Attribute("data-basename", Text(currentValue.collectionControlId + "-item"),
    		  Null))
      <div class="coll-list-input">
        <ul id={currentValue.collectionControlId} class="sortableList">{
          if (currentValue.v.isDefined) {
            val cv = currentValue.v.get.cv
            cv.zipWithIndex.map { pair =>
              val (elemV, i) = pair
              val itemRendered = elemT.renderInput(prop, state, currentValue, elemV) %
              	Attribute("id", Text(currentValue.collectionControlId + "-item[" + i + "]"), 
              	Attribute("name", Text(currentValue.collectionControlId + "-item[" + i + "]"), Null))
              <li><span class="ui-icon ui-icon-arrowthick-2-n-s"></span>{itemRendered}</li>
            }
          }
        }</ul>
        <button class="add-item-button btn-mini" data-size={currentValue.collectionControlId + "-size"}>&nbsp;</button>
        <input type="hidden" id={currentValue.collectionControlId + "-size"} value={currentValue.v.map(_.cv.size).getOrElse(0).toString}/>
        {inputTemplate}
      </div>
    }
    
    import play.api.data.Form
    // TODO: this will want to be refactored with the default version in Collection.scala
    override def fromUser(form:Form[_], prop:Property[_,_], elemT:pType):FormFieldInfo = {
      val fieldId = prop.id.toString
      val empty = form("empty-" + fieldId).value map (_.toBoolean) getOrElse false
      if (empty) {
        FormFieldInfo(prop, None, true, true)
      } else {
        val oldListName = "coll-" + fieldId + "-item"
        val oldList = form(oldListName)
        val oldIndexes = oldList.indexes
        val oldRaw =
          for (i <- oldIndexes;
               v <- oldList("[" + i + "]").value)
            yield v
        val oldVals = oldRaw.map(elemT.fromUser(_)).toList
        FormFieldInfo(prop, Some(makePropValue(oldVals)), false, true)
      }
    }
  }
  object QList extends QList(QListOID)
  
object SystemCollections {
  def all = Space.oidMap[Collection](UrCollection, ExactlyOne, Optional, QList)
}