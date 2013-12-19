package models.system

import language.existentials
import scala.xml._

import play.api.Logger

// TODO: solely for fromUser, which really should get moved elsewhere:
import play.api.data.Form

import models._

import Thing._

import OIDs._

import ql._

import querki.util._
import querki.values._

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
        setName("Collection"),
        InternalProp(true)
        )) 
  {
	type implType = List[ElemValue]
	
    def doDeserialize(ser:String, elemT:pType):implType = 
      throw new Error("Trying to deserialize root collection!")
    def doSerialize(v:implType, elemT:pType):String = 
      throw new Error("Trying to serialize root collection!")
    def doWikify(context:QLContext)(ser:implType, elemT:pType, displayOpt:Option[Wikitext] = None):Wikitext = 
      throw new Error("Trying to render root collection!")
    def doDefault(elemT:pType):implType = 
      throw new Error("Trying to default root collection!")    
	def wrap(elem:ElemValue):implType =
	  throw new Error("Trying to wrap root collection!")    
	def makePropValue(cv:Iterable[ElemValue], pType:PType[_]):QValue =
	  throw new Error("Trying to makePropValue root collection!")    
    def doRenderInput(prop:Property[_,_], state:SpaceState, currentValue:DisplayPropVal, elemT:PType[_]):scala.xml.Elem =
	  throw new Error("Trying to render input on root collection!")
	def fromUser(on:Option[Thing], form:Form[_], prop:Property[_,_], elemT:pType, state:SpaceState):FormFieldInfo =
	  throw new Error("Trying to fromUser on root collection!")
  }
  object UrCollection extends UrCollection
  
abstract class SingleElementBase(cid:OID, pf:PropFetcher) extends SystemCollection(cid, pf)
{
  // TODO: this really doesn't belong here. We need to tease the HTTP/HTML specific
  // stuff out from the core concepts.
  // TODO: this will need refactoring, to get more complex on a per-Collection basis
  def fromUser(on:Option[Thing], form:Form[_], prop:Property[_,_], elemT:pType, state:SpaceState):FormFieldInfo = {
    val fieldIds = FieldIds(on, prop)
    val empty = form(fieldIds.emptyControlId).value map (_.toBoolean) getOrElse false
    if (empty) {
      FormFieldInfo(prop, None, true, true)
    } else {
      val formV = form(fieldIds.inputControlId).value
      formV match {
    	// Normal case: pass it to the PType for parsing the value out:
        case Some(v) => {
          rawInterpretation(v, prop, elemT).getOrElse {
            TryTrans { elemT.validate(v, prop, state) }.
              onSucc { _ => FormFieldInfo(prop, Some(apply(elemT.fromUser(v))), false, true, Some(v)) }.
              onFail { ex => FormFieldInfo(prop, None, true, false, Some(v), Some(ex)) }.
              result
          }
        }
        // There was no field value found. In this case, we take the default. That
        // seems strange, but this case is entirely valid in the case of a checkbox.
        // IMPORTANT / TODO: this code is horribly specific to the weird edge case of
        // checkboxes! I don't love it, and it needs heavy testing!
        case None => FormFieldInfo(prop, Some(apply(elemT.default)), false, true)
      }
    }
  }
  
  def rawInterpretation(v:String, prop:Property[_,_], elemT:pType):Option[FormFieldInfo] = None
}
  
  /**
   * ExactlyOne is essentially Some -- it is quite intentionally Optional without the choice of None.
   * 
   * Sadly, though, Option isn't actually an Iterable, and trying to use type views to do this
   * was making me nuts.
   * 
   * TODO: rewrite ExactlyOne and Optional to be based on an actual Iterable with the right semantics.
   */
  class ExactlyOne(cid:OID) extends SingleElementBase(cid,
    ExactlyOneProps.fetchProps) 
  {
    type implType = List[ElemValue]

	def doDeserialize(ser:String, elemT:pType):implType = {
      List(elemT.deserialize(ser))
    }
    def doSerialize(v:implType, elemT:pType):String = {
      elemT.serialize(v.head)
    }
    def doWikify(context:QLContext)(v:implType, elemT:pType, displayOpt:Option[Wikitext] = None):Wikitext = {
      elemT.wikify(context)(v.head, displayOpt)
    }
    def doDefault(elemT:pType):implType = {
      List(elemT.default)
    }
    def wrap(elem:ElemValue):implType = List(elem)
    
    def doRenderInput(prop:Property[_,_], state:SpaceState, currentValue:DisplayPropVal, elemT:PType[_]):scala.xml.Elem = {
      val v = currentValue.effectiveV.flatMap(_.firstOpt).getOrElse(elemT.default)
      elemT.renderInput(prop, state, currentValue, v)
    }

    def makePropValue(cv:Iterable[ElemValue], elemT:PType[_]):QValue = ExactlyOnePropValue(cv.toList, this, elemT)
    protected case class ExactlyOnePropValue(cv:implType, cType:ExactlyOne, pType:PType[_]) extends QValue
  }
  object ExactlyOne extends ExactlyOne(ExactlyOneOID)
  object ExactlyOneProps {
    def fetchProps:() => PropMap = {
      Thing.toProps(
        setName("Exactly-One")
      )
    }    
  }
  
  class Optional(cid:OID) extends SingleElementBase(cid,
      toProps(
        setName("Optional")
        )) 
  {
    type implType = List[ElemValue]
    
    override def rawInterpretation(v:String, prop:Property[_,_], elemT:pType):Option[FormFieldInfo] = {
      // If the input was empty, that's QNone.
      // TODO: this isn't good enough for the long run -- we'll have to do something more
      // sophisticated when we get to complex Types. But it's a start.
      if (v.length() == 0)
        Some(FormFieldInfo(prop, Some(QNone), false, true))
      else
        None
    }
    
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
    
    def doWikify(context:QLContext)(v:implType, elemT:pType, displayOpt:Option[Wikitext] = None):Wikitext = {
      v match {
        case List(elem) => elemT.wikify(context)(elem, displayOpt)
        case Nil => Wikitext("")
      }
    }
    
    def doDefault(elemT:pType):implType = Nil
    
    def wrap(elem:ElemValue):implType = List(elem)
    def makePropValue(cv:Iterable[ElemValue], elemT:PType[_]):QValue = OptionalPropValue(cv.toList, this, elemT)    
    private case class OptionalPropValue(cv:implType, cType:Optional, pType:PType[_]) extends QValue
    
    def doRenderInput(prop:Property[_,_], state:SpaceState, currentValue:DisplayPropVal, elemT:PType[_]):scala.xml.Elem = {
      // TODO: what should we do here? Has custom rendering become unnecessary here? Does the appearance of the
      // trash button eliminate the need for anything fancy for Optional properties?
      val v = currentValue.effectiveV.flatMap(_.firstOpt).getOrElse(elemT.default)
      elemT.renderInput(prop, state, currentValue, v)
    }

    val QNone:QValue = makePropValue(Nil, UnknownType)
    def Empty(elemT:pType):QValue = makePropValue(Nil, elemT) 
  }
  object Optional extends Optional(OptionalOID)
  
  
  abstract class QListBase(cid:OID, pf:PropFetcher) extends SystemCollection(cid, pf) 
  {
    type implType = List[ElemValue]
    
    def doDeserialize(ser:String, elemT:pType):implType = {
      val guts = ser.slice(1, ser.length() - 1).trim()
      if (guts.isEmpty())
        doDefault(elemT)
      else {
        val temp = "" + Char.MinValue
        val elemStrs = guts.replace("\\,", temp).split(",").toList.map(_.replace(temp, ","))
        elemStrs.map(elemT.deserialize(_))
      }
    }
    
    def doSerialize(v:implType, elemT:pType):String = {
      v.map(elem => elemT.serialize(elem)).
        map(_.replace(",", "\\,")).
        mkString("[", "," ,"]")
    }
    
    def doWikify(context:QLContext)(v:implType, elemT:pType, displayOpt:Option[Wikitext] = None):Wikitext = {
      val renderedElems = v.map(elem => elemT.wikify(context)(elem, displayOpt))
      // Concatenate the rendered elements, with newlines in-between:
      (Wikitext.empty /: renderedElems) ((soFar, next) => soFar.+(next, true))
    }
    
    def doDefault(elemT:pType):implType = List.empty
    
    def wrap(elem:ElemValue):implType = List(elem)
    
    val empty = makePropValue(List.empty[ElemValue], UnknownType)
    def empty(elemT:pType):QValue = makePropValue(List.empty[ElemValue], elemT)   

    // TODO: this stuff is QList-specific. We'll want something different for QSet, but much of that is
    // already in HtmlRenderer.
    // TODO: the stuff created here overlaps badly with the Javascript code in editThing.scala.html.
    // Rationalize the two, to eliminate all the duplication. In theory, the concept and structure
    // belongs here, and the details belong there.
    def doRenderInput(prop:Property[_,_], state:SpaceState, currentValue:DisplayPropVal, elemT:PType[_]):scala.xml.Elem = {
      import querki.html.HtmlRenderer
      val inputTemplate = HtmlRenderer.addClasses(elemT.renderInput(prop, state, currentValue, elemT.default), "inputTemplate list-input-element") %      
    		  Attribute("data-basename", Text(currentValue.collectionControlId + "-item"),
    		  Null)
      val addButtonId = currentValue.collectionControlId + "-addButton"
      <div class="coll-list-input" data-delegate-disable-to={addButtonId}>
        <ul id={currentValue.collectionControlId} class="sortableList">{
          currentValue.effectiveV.map { v =>
            val cv = v.cv
            cv.zipWithIndex.map { pair =>
              val (elemV, i) = pair
              val simplyRendered = elemT.renderInput(prop, state, currentValue, elemV)
              val itemRendered = HtmlRenderer.addClasses(simplyRendered, "list-input-element") %
              	Attribute("id", Text(currentValue.collectionControlId + "-item[" + i + "]"), 
              	Attribute("name", Text(currentValue.collectionControlId + "-item[" + i + "]"), Null))
              <li><span class="icon-move"></span>{itemRendered}<button class="delete-item-button btn-mini">&nbsp;</button></li>
            }
          }.getOrElse(NodeSeq.Empty)
        }</ul>
        <button class="add-item-button btn-mini" id={addButtonId} data-size={currentValue.collectionControlId + "-size"}>&nbsp;</button>
        <input type="hidden" id={currentValue.collectionControlId + "-size"} value={currentValue.v.map(_.cv.size).getOrElse(0).toString}/>
        {inputTemplate}
      </div>
    }
    
    import play.api.data.Form
    // TODO: this will want to be refactored with the default version in Collection.scala
    override def fromUser(on:Option[Thing], form:Form[_], prop:Property[_,_], elemT:pType, state:SpaceState):FormFieldInfo = {
      val fieldIds = FieldIds(on, prop)
      val empty = form(fieldIds.emptyControlId).value map (_.toBoolean) getOrElse false
      if (empty) {
        FormFieldInfo(prop, None, true, true)
      } else {
        val oldListName = fieldIds.collectionControlId + "-item"
        val oldList = form(oldListName)
        val oldIndexes = oldList.indexes
        val oldRaw =
          for (i <- oldIndexes;
               v <- oldList("[" + i + "]").value)
            yield v
        val oldVals = oldRaw.map(elemT.fromUser(_)).toList
        FormFieldInfo(prop, Some(makePropValue(oldVals, elemT)), false, true)
      }
    }
  }
  class QList(cid:OID) extends QListBase(cid,
      toProps(
        setName("List")
        ))
  {
    def makePropValue(cv:Iterable[ElemValue], elemT:PType[_]):QValue = QListPropValue(cv.toList, this, elemT)
    protected case class QListPropValue(cv:implType, cType:QList, pType:PType[_]) extends QValue    
    
    /**
     * Given an incoming Iterable of RTs, this produces the corresponding QList of VTs.
     * This should simplify a lot of the Scala-level code.
     */
    def from[RT,VT](in:Iterable[RT], builder:PTypeBuilderBase[VT,RT]):QValue = {
      val rawList = (List.empty[ElemValue] /: in)((list, next) => list :+ builder(next))
      makePropValue(rawList, builder.pType)
    }
  }
  object QList extends QList(QListOID)
  
  class QSet(cid:OID) extends QListBase(cid,
      toProps(
        setName("Set")))
  {
    // TODO: this *really* should be makePropValue -- it is Very Very Bad that it isn't. But
    // that doesn't yet have a way of getting at the PType, which we need for comp() and matches().
    // This may become less critical once ElemValue carries the PType.
    def makeSetValue(rawList:implType, pt:PType[_], context:QLContext):QValue = {
      val sorted = rawList.sortWith(pt.comp(context))
      val deduped = ((List.empty[ElemValue], Option.empty[ElemValue]) /: sorted){ (state, next) =>
        val (list, prevOpt) = state
        prevOpt match {
          case Some(prev) =>
            if (pt.matches(prev, next))
              state
            else
              (list :+ next, Some(next))
          case None => (list :+ next, Some(next))
        }
      }

      QSetPropValue(deduped._1, this, pt)
    }
    def makePropValue(cv:Iterable[ElemValue], elemT:PType[_]):QValue = QSetPropValue(cv.toList, this, elemT)
    private case class QSetPropValue(cv:implType, cType:QSet, pType:PType[_]) extends QValue    
  }
  object QSet extends QSet(QSetOID)
  
/**
 * This is a special marker collection that is Unit -- that is, it is by definition empty.
 * It should only be used for "marker" Properties, where the existence or non-existence of the
 * Property is the significant part. Action-only side-effecting Methods are the most likely
 * usage.
 */
class QUnit(cid:OID) extends SystemCollection(cid,
  toProps(
    setName("Always Empty"),
    InternalProp(true)
  )) 
{
  type implType = List[ElemValue]
    
  def doDeserialize(ser:String, elemT:pType):implType = Nil
    
  def doSerialize(v:implType, elemT:pType):String = ""
    
  def doWikify(context:QLContext)(v:implType, elemT:pType, displayOpt:Option[Wikitext] = None):Wikitext = Wikitext("")
    
  def doDefault(elemT:pType):implType = Nil
    
  def wrap(elem:ElemValue):implType = Nil
  def makePropValue(cv:Iterable[ElemValue], elemT:PType[_]):QValue = UnitPropValue(cv.toList, this, elemT)    
  private case class UnitPropValue(cv:implType, cType:QUnit, pType:PType[_]) extends QValue
    
  def doRenderInput(prop:Property[_,_], state:SpaceState, currentValue:DisplayPropVal, elemT:PType[_]):scala.xml.Elem = {
    <i>Defined</i>
  }

  def fromUser(on:Option[Thing], form:Form[_], prop:Property[_,_], elemT:pType, state:SpaceState):FormFieldInfo =
	throw new Error("Trying to fromUser on Unit!")
}
object QUnit extends QUnit(QUnitOID)
    
object SystemCollections {
  def all = OIDMap[Collection](UrCollection, ExactlyOne, Optional, QList, QSet, QUnit)
}