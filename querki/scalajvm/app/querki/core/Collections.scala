package querki.core

import language.existentials
import scala.xml.{Attribute, NodeSeq, Null, Text}

import play.api.Logger

// TODO: solely for fromUser, which really should get moved elsewhere:
import play.api.data.Form

import models._

import querki.api.commonName
import querki.ecology._
import querki.globals._
import querki.util.{TryTrans, XmlHelpers}
import querki.values._

import MOIDs._

//////////////////////////////////////
//
// Collections
//

trait CollectionBase { self:CoreEcot =>
  def UnknownType:PType[Unit]
  
  abstract class SystemCollection(cid:OID, pf:PropMap)(implicit e:Ecology) extends Collection(cid, systemOID, UrCollectionOID, pf)

  abstract private[core] class SingleElementBase(cid:OID, pf:PropMap)(implicit e:Ecology) extends SystemCollection(cid, pf)(e)
  {
    // TODO: this really doesn't belong here. We need to tease the HTTP/HTML specific
    // stuff out from the core concepts.
    // TODO: this will need refactoring, to get more complex on a per-Collection basis
    def fromUser(on:Option[Thing], form:Form[_], prop:Property[_,_], elemT:pType, containers:Option[FieldIds], state:SpaceState):FormFieldInfo = {
      implicit val s = state
      val fieldIds = new FieldIds(on, prop, containers)
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
    
    def fromUserString(prop:AnyProp, v:String, elemT:pType, state:SpaceState):FormFieldInfo = {
      implicit val s = state
      rawInterpretation(v, prop, elemT).getOrElse {
          TryTrans { elemT.validate(v, prop, state) }.
            onSucc { _ => FormFieldInfo(prop, Some(apply(elemT.fromUser(v))), false, true, Some(v)) }.
            onFail { ex => FormFieldInfo(prop, None, true, false, Some(v), Some(ex)) }.
            result
      }
    }
    
    def fromUser(info:FieldIds, vs:List[String], state:SpaceState):FormFieldInfo = {
      val prop = info.p
      val elemT = prop.pType
      vs.headOption match {
        case Some(v) => fromUserString(prop, v, elemT, state)
        case None => {
          QLog.error(s"ExactlyOne.fromUser() received an empty list of values!")
          FormFieldInfo(prop, Some(apply(elemT.default(state))), true, true)
        }
      }
    }
  }
  
  /**
   * ExactlyOne is essentially Some -- it is quite intentionally Optional without the choice of None.
   * 
   * Sadly, though, Option isn't actually an Iterable, and trying to use type views to do this
   * was making me nuts.
   * 
   * TODO: rewrite ExactlyOne and Optional to be based on an actual Iterable with the right semantics.
   */
  class ExactlyOneBase(oid:OID) extends SingleElementBase(oid, 
      toProps(
        setName(commonName(_.core.exactlyOneColl)))) 
  {
    type implType = List[ElemValue]

	def doDeserialize(ser:String, elemT:pType)(implicit state:SpaceState):implType = {
      List(elemT.deserialize(ser))
    }
    def doSerialize(v:implType, elemT:pType)(implicit state:SpaceState):String = {
      elemT.serialize(v.headOption.getOrElse(elemT.default))
    }
    def doWikify(context:QLContext)(v:implType, elemT:pType, displayOpt:Option[Wikitext] = None, lexicalThing:Option[PropertyBundle] = None):Future[Wikitext] = {
      elemT.wikify(context)(v.headOption.getOrElse(elemT.default(context.state)), displayOpt, lexicalThing)
    }
    def doDefault(elemT:pType)(implicit state:SpaceState):implType = {
      List(elemT.default)
    }
    def wrap(elem:ElemValue):implType = List(elem)
    
    def doRenderInput(prop:Property[_,_], context:QLContext, currentValue:DisplayPropVal, elemT:PType[_]):Future[NodeSeq] = {
      implicit val s = context.state
      val v = currentValue.effectiveV.flatMap(_.firstOpt).getOrElse(elemT.default)
      elemT.renderInput(prop, context, currentValue, v)
    }

    def makePropValue(cv:Iterable[ElemValue], elemT:PType[_]):QValue = ExactlyOnePropValue(cv.toList, this, elemT)
    protected case class ExactlyOnePropValue(cv:implType, cType:ExactlyOneBase, pType:PType[_]) extends QValue
    
    def append(v:implType, elem:ElemValue):(QValue,Option[ElemValue]) = {
      val old = v.headOption
      (makePropValue(List(elem), elem.pType), old)
    }
  }
  
  abstract class QListBase(cid:OID, pf:PropMap) extends SystemCollection(cid, pf) 
  {
    type implType = List[ElemValue]
    
    def doDeserialize(ser:String, elemT:pType)(implicit state:SpaceState):implType = {
      val guts = 
        // Note: this is a bit of a hack. We've had at least one Issue (.3y28amy) where we accidentally
        // stored Set data as ExactlyOne. As a result, the storage format was wrong. We don't want to
        // lose data, so for now we're making this a bit forgiving. This is kind of awful, but given
        // that this storage engine isn't long for this world anyway, we'll do it.
        if (ser.head == '[' && ser.last == ']')
          ser.slice(1, ser.length() - 1).trim()
        else
          ser
      if (guts.isEmpty())
        doDefault(elemT)
      else {
        val temp = "" + Char.MinValue
        val elemStrs = guts.replace("\\,", temp).split(",").toList.map(_.replace(temp, ","))
        elemStrs.map(elemT.deserialize(_))
      }
    }
    
    def doSerialize(v:implType, elemT:pType)(implicit state:SpaceState):String = {
      v.map(elem => elemT.serialize(elem)).
        map(_.replace(",", "\\,")).
        mkString("[", "," ,"]")
    }
    
    def doWikify(context:QLContext)(v:implType, elemT:pType, displayOpt:Option[Wikitext] = None, lexicalThing:Option[PropertyBundle] = None):Future[Wikitext] = {
      Future.sequence(v.map(elem => elemT.wikify(context)(elem, displayOpt, lexicalThing))) map { renderedElems =>
        // Concatenate the rendered elements, with newlines in-between:
        (Wikitext.empty /: renderedElems) ((soFar, next) => soFar.+(next, true))        
      }
    }
    
    def doDefault(elemT:pType)(implicit state:SpaceState):implType = List.empty
    
    def wrap(elem:ElemValue):implType = List(elem)
    
    val empty = makePropValue(List.empty[ElemValue], UnknownType)
    def empty(elemT:pType):QValue = makePropValue(List.empty[ElemValue], elemT)   

    // TODO: this stuff is QList-specific. We'll want something different for QSet, but much of that is
    // already in HtmlRenderer.
    // TODO: the stuff created here overlaps badly with the Javascript code in editThing.scala.html.
    // Rationalize the two, to eliminate all the duplication. In theory, the concept and structure
    // belongs here, and the details belong there.
    def doRenderInput(prop:Property[_,_], context:QLContext, currentValue:DisplayPropVal, elemT:PType[_]):Future[NodeSeq] = {
      implicit val state = context.state
      val HtmlRenderer = interface[querki.html.HtmlRenderer]
      for {
        defaultElem <- elemT.renderInput(prop, context, currentValue.copy(i = Some(-1)), elemT.default)
        defaulted = HtmlRenderer.addClasses(defaultElem, "inputTemplate list-input-element")
        inputTemplate = XmlHelpers.mapElems(defaulted) ( _ %      
          Attribute("data-basename", Text(currentValue.collectionControlId + "-item"),
          Null))
        addButtonId = currentValue.collectionControlId + "-addButton"
        gutsses =
          currentValue.effectiveV.map { v =>
            val cv = v.cv
            cv.zipWithIndex.map { pair =>
              val (elemV, i) = pair
              val elemCurrentValue = currentValue.copy(i = Some(i))
              elemT.renderInput(prop, context, elemCurrentValue, elemV).map { simplyRendered =>
                val withClasses = HtmlRenderer.addClasses(simplyRendered, "list-input-element")
                val itemRendered:NodeSeq = XmlHelpers.mapElems(withClasses) { elem =>
                  elem %
                    Attribute("id", Text(currentValue.collectionControlId + "-item[" + i + "]"), 
                    Attribute("name", Text(currentValue.collectionControlId + "-item[" + i + "]"), Null))
                }
                <li class="list-input-item">{itemRendered}</li>                
              }
            }
          }.getOrElse(Seq(fut(NodeSeq.Empty)))
        guts <- Future.sequence(gutsses)
      }
      yield 
        <div class="coll-list-input" data-delegate-disable-to={addButtonId}>
          <ul id={currentValue.collectionControlId} class="sortableList">{guts}</ul>
          <button class="add-item-button btn-xs" id={addButtonId} data-size={currentValue.collectionControlId + "-size"}>&nbsp;</button>
          <input type="hidden" id={currentValue.collectionControlId + "-size"} value={currentValue.v.map(_.cv.size).getOrElse(0).toString}/>
          {inputTemplate}
        </div>
    }
    
    import play.api.data.Form
    // TODO: this will want to be refactored with the default version in Collection.scala
    override def fromUser(on:Option[Thing], form:Form[_], prop:Property[_,_], elemT:pType, containers:Option[FieldIds], state:SpaceState):FormFieldInfo = {
      implicit val s = state
      val fieldIds = new FieldIds(on, prop, containers)
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
    protected case class QListPropValue(cv:implType, cType:QListBase, pType:PType[_]) extends QValue    
    def makePropValue(cv:Iterable[ElemValue], elemT:PType[_]):QValue = QListPropValue(cv.toList, this, elemT)
    
    def fromUser(info:FieldIds, vs:List[String], state:SpaceState):FormFieldInfo = {
      val prop = info.p
      val elemT = prop.pType
      implicit val s = state
      
      val elems = info.listIndex match {
        case Some(i) => {
          // This should be replacing a single element, so we need to splice it into the existing list:
          val result = for {
            v <- vs.headOption
            t <- info.bundleOpt
            oldPV <- t.getPropOpt(prop)
            oldList = oldPV.v.cv.toList
            newElem = elemT.fromUser(v)
          }
            yield oldList.updated(i, newElem)
            
          result.getOrElse(throw new Exception(s"QList.fromUser got inconsistent fieldIds $info with values $vs"))
        }
        case None => vs.map(elemT.fromUser(_)).toList
      } 
        
      FormFieldInfo(prop, Some(makePropValue(elems, elemT)), false, true)
    }
    
    def append(v:implType, elem:ElemValue):(QValue,Option[ElemValue]) = {
      (makePropValue(v :+ elem, elem.pType), None)
    }
  }

}

trait CollectionCreation { self:CoreEcot with CollectionBase with CoreExtra =>

  /**
   * Root Collection type. Exists solely so that there is a common runtime root, in case
   * we want to be able to write new collections.
   */
  class UrCollection extends Collection(UrCollectionOID, systemOID, querki.core.MOIDs.RootOID,
      toProps(
        setName("Collection"),
        setInternal
        ))
  {
	  type implType = List[ElemValue]
	
    def doDeserialize(ser:String, elemT:pType)(implicit state:SpaceState):implType = 
      throw new Error("Trying to deserialize root collection!")
    def doSerialize(v:implType, elemT:pType)(implicit state:SpaceState):String = 
      throw new Error("Trying to serialize root collection!")
    def doWikify(context:QLContext)(ser:implType, elemT:pType, displayOpt:Option[Wikitext] = None, lexicalThing:Option[PropertyBundle] = None):Future[Wikitext] = 
      throw new Error("Trying to render root collection!")
    def doDefault(elemT:pType)(implicit state:SpaceState):implType = 
      throw new Error("Trying to default root collection!")    
  	def wrap(elem:ElemValue):implType =
  	  throw new Error("Trying to wrap root collection!")    
  	def makePropValue(cv:Iterable[ElemValue], pType:PType[_]):QValue =
  	  throw new Error("Trying to makePropValue root collection!")    
    def doRenderInput(prop:Property[_,_], context:QLContext, currentValue:DisplayPropVal, elemT:PType[_]):Future[NodeSeq] =
  	  throw new Error("Trying to render input on root collection!")
  	def fromUser(on:Option[Thing], form:Form[_], prop:Property[_,_], elemT:pType, containers:Option[FieldIds], state:SpaceState):FormFieldInfo =
  	  throw new Error("Trying to fromUser on root collection!")
  	def append(v:implType, elem:ElemValue):(QValue,Option[ElemValue]) = ???
  	def fromUser(info:FieldIds, vs:List[String], state:SpaceState):FormFieldInfo = ???
  }
  
  class ExactlyOne(implicit e:Ecology) extends ExactlyOneBase(ExactlyOneOID)

  class Optional extends SingleElementBase(OptionalOID,
      toProps(
        setName(commonName(_.core.optionalColl))
        )) 
  {
    type implType = List[ElemValue]
    
    override def rawInterpretation(v:String, prop:Property[_,_], elemT:pType):Option[FormFieldInfo] = {
      // If the input was empty, that's QNone.
      // TODO: this isn't good enough for the long run -- we'll have to do something more
      // sophisticated when we get to complex Types. But it's a start.
      if (v.length() == 0)
        Some(FormFieldInfo(prop, Some(Empty(elemT)), false, true))
      else
        None
    }
    
    def doDeserialize(ser:String, elemT:pType)(implicit state:SpaceState):implType = {
      ser match {
        case "!" => Nil
        case s:String => {
          val elemStr = s.slice(1, s.length() - 1)
          List(elemT.deserialize(elemStr))
        }
      }
    }
    
    def doSerialize(v:implType, elemT:pType)(implicit state:SpaceState):String = {
      v match {
        case List(elem) => "(" + elemT.serialize(elem) + ")"
        case Nil => "!"
      }
    }
    
    def doWikify(context:QLContext)(v:implType, elemT:pType, displayOpt:Option[Wikitext] = None, lexicalThing:Option[PropertyBundle] = None):Future[Wikitext] = {
      v match {
        case List(elem) => elemT.wikify(context)(elem, displayOpt, lexicalThing)
        case Nil => fut(Wikitext(""))
      }
    }
    
    def doDefault(elemT:pType)(implicit state:SpaceState):implType = Nil
    
    def wrap(elem:ElemValue):implType = List(elem)
    def makePropValue(cv:Iterable[ElemValue], elemT:PType[_]):QValue = OptionalPropValue(cv.toList, this, elemT)    
    private case class OptionalPropValue(cv:implType, cType:Optional, pType:PType[_]) extends QValue
    
    def doRenderInput(prop:Property[_,_], context:QLContext, currentValue:DisplayPropVal, elemT:PType[_]):Future[NodeSeq] = {
      implicit val state = context.state
      // TODO: what should we do here? Has custom rendering become unnecessary here? Does the appearance of the
      // trash button eliminate the need for anything fancy for Optional properties?
      val v = currentValue.effectiveV.flatMap(_.firstOpt).getOrElse(elemT.default)
      elemT.renderInput(prop, context, currentValue, v)
    }

    val QNone:QValue = makePropValue(Nil, UnknownType)
    def Empty(elemT:pType):QValue = makePropValue(Nil, elemT)
        
    def append(v:implType, elem:ElemValue):(QValue,Option[ElemValue]) = {
      val old = v.headOption
      (makePropValue(List(elem), elem.pType), old)
    }
    
    override def fromUser(info:FieldIds, vs:List[String], state:SpaceState):FormFieldInfo = {
      val prop = info.p
      val elemT = prop.pType
      vs.headOption match {
        case Some(v) => fromUserString(prop, v, elemT, state)
        case None => FormFieldInfo(prop, Some(Empty(elemT)), false, true)
      }
    }
  }
  
  class QList extends QListBase(QListOID,
      toProps(
        setName(commonName(_.core.listColl))
        ))
  {
    /**
     * Given an incoming Iterable of RTs, this produces the corresponding QList of VTs.
     * This should simplify a lot of the Scala-level code.
     */
    def from[RT,VT](in:Iterable[RT], builder:PTypeBuilderBase[VT,RT]):QValue = {
      val rawList = (List.empty[ElemValue] /: in)((list, next) => list :+ builder(next))
      makePropValue(rawList, builder.pType)
    }
  }
  
  class QSet extends QListBase(QSetOID,
      toProps(
        setName(commonName(_.core.setColl))))
  {
    // TODO: this *really* should be makePropValue -- it is Very Very Bad that it isn't. But
    // that doesn't yet have a way of getting at the PType, which we need for comp() and matches().
    // This may become less critical once ElemValue carries the PType.
    def makeSetValue(rawList:Seq[ElemValue], pt:PType[_], context:QLContext):QValue = {
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
    override def makePropValue(cv:Iterable[ElemValue], elemT:PType[_]):QValue = QSetPropValue(cv.toList, this, elemT)
    private case class QSetPropValue(cv:implType, cType:QSet, pType:PType[_]) extends QValue    
  }
  
  /**
   * This is a special marker collection that is Unit -- that is, it is by definition empty.
   * It should only be used for "marker" Properties, where the existence or non-existence of the
   * Property is the significant part. Action-only side-effecting Methods are the most likely
   * usage.
   */
  class QUnit extends SystemCollection(QUnitOID,
    toProps(
      setName("Always Empty"),
      setInternal
    )) 
  {
    type implType = List[ElemValue]
    
    def doDeserialize(ser:String, elemT:pType)(implicit state:SpaceState):implType = Nil
    
    def doSerialize(v:implType, elemT:pType)(implicit state:SpaceState):String = ""
    
    def doWikify(context:QLContext)(v:implType, elemT:pType, displayOpt:Option[Wikitext] = None, lexicalThing:Option[PropertyBundle] = None):Future[Wikitext] = 
      fut(Wikitext(""))
    
    def doDefault(elemT:pType)(implicit state:SpaceState):implType = Nil
    
    def wrap(elem:ElemValue):implType = Nil
    def makePropValue(cv:Iterable[ElemValue], elemT:PType[_]):QValue = UnitPropValue(cv.toList, this, elemT)    
    private case class UnitPropValue(cv:implType, cType:QUnit, pType:PType[_]) extends QValue
    
    def doRenderInput(prop:Property[_,_], context:QLContext, currentValue:DisplayPropVal, elemT:PType[_]):Future[NodeSeq] = {
      fut(<i>Defined</i>)
    }

    def fromUser(on:Option[Thing], form:Form[_], prop:Property[_,_], elemT:pType, containers:Option[FieldIds], state:SpaceState):FormFieldInfo =
	  throw new Error("Trying to fromUser on Unit!")
    
    def append(v:implType, elem:ElemValue):(QValue,Option[ElemValue]) = ???
    
    def fromUser(info:FieldIds, vs:List[String], state:SpaceState):FormFieldInfo = ???
  }

  /**
   * A boot-time singleton collection, which exists in order to hold the first few key Properties.
   * 
   * This should not be used outside of Core, and not even heavily used in Core. It cannot be serialized; moreover,
   * nothing that *uses* it can be serialized, so it is strictly for system objects. It basically exists so that
   * we can get the real Collections booted up.
   */
  class bootCollection extends SingleElementBase(UnknownOID, models.Thing.emptyProps) {
    type implType = List[ElemValue]

    def doDeserialize(ser:String, elemT:pType)(implicit state:SpaceState):implType = List(elemT.deserialize(ser))

    def doSerialize(v:implType, elemT:pType)(implicit state:SpaceState):String = elemT.serialize(v.head)

    def doWikify(context:QLContext)(v:implType, elemT:pType, displayOpt:Option[Wikitext] = None, lexicalThing:Option[PropertyBundle] = None):Future[Wikitext] = {
      elemT.wikify(context)(v.head, displayOpt, lexicalThing)
    }
    def doDefault(elemT:pType)(implicit state:SpaceState):implType = {
      List(elemT.default)
    }
    def wrap(elem:ElemValue):implType = List(elem)
    def makePropValue(cv:Iterable[ElemValue], elemT:PType[_]):QValue = bootPropValue(cv.toList, this, elemT)
    
    def doRenderInput(prop:Property[_,_], context:QLContext, currentValue:DisplayPropVal, elemT:PType[_]):Future[NodeSeq] = {
      implicit val s = context.state
      val v = currentValue.v.map(_.first).getOrElse(elemT.default)
      elemT.renderInput(prop, context, currentValue, v)
    }

    private case class bootPropValue(cv:implType, cType:bootCollection, pType:PType[_]) extends QValue {}  
       
    def append(v:implType, elem:ElemValue):(QValue,Option[ElemValue]) = {
      val old = v.headOption
      (makePropValue(List(elem), elem.pType), old)
    }
  }
}
