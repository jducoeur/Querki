package querki.values

import models._
import models.system._

// TODO: both of these should get evolved away!
import play.api.Logger
import play.api.templates.Html

import querki.util._
import querki.values._

/**
 * This is a fake PType, which exists so that we can persist embedded Texts in the pipeline.
 */
object ParsedTextType extends SystemType[Wikitext](OIDs.IllegalOID, () => Thing.emptyProps) with SimplePTypeBuilder[Wikitext]
{
  def doDeserialize(v:String) = throw new Exception("Can't deserialize ParsedText!")
  def doSerialize(v:Wikitext) = throw new Exception("Can't serialize ParsedText!")
  def doWikify(context:QLContext)(v:Wikitext, displayOpt:Option[Wikitext] = None) = v
  
  override def doDebugRender(context:QLContext)(v:Wikitext) = v.contents.map(_.internal).mkString
  
  val doDefault = Wikitext("")
  def wrap(raw:String):valType = Wikitext(raw)
}

/**
 * This is a fake PType, so that code can inject HTML into the pipeline
 */
object RawHtmlType extends SystemType[Wikitext](OIDs.IllegalOID, () => Thing.emptyProps) with SimplePTypeBuilder[Wikitext]
{
  def doDeserialize(v:String) = throw new Exception("Can't deserialize ParsedText!")
  def doSerialize(v:Wikitext) = throw new Exception("Can't serialize ParsedText!")
  def doWikify(context:QLContext)(v:Wikitext, displayOpt:Option[Wikitext] = None) = v
    
  val doDefault = Wikitext("")
}

/**
 * This is a fake PType, used when we encounter a name we don't know.
 */
object UnknownNameType extends NameType(UnknownOID, "_unknownNameType") {
  def doWikify(context:QLContext)(v:String, displayOpt:Option[Wikitext] = None) = {
    Wikitext("{{_unknownName:") + nameToLink(context)(v, displayOpt) + Wikitext("}}")
  }
}

/**
 * Marker trait, to indicate that we should stop processing at this value. Mix it
 * into the returned value to indicate that we should stop. This is probably a stopgap,
 * but it's okay for now.
 */
trait CutProcessing

trait QValue {
  // We are cutting iff the constructor mixed in CutProcessing:
  def cut = this.isInstanceOf[CutProcessing]
  
  val cType:Collection
  type cType = cType.implType
  def cv:cType
  
  def pType:PType[_]
  
  // TODO: this doesn't need to take elemT any more:
  def serialize(elemT:PType[_]):String = cType.doSerialize(cv, elemT)
  // DEPRECATED: in favor of firstOpt
  def first = cType.first(this)
  def firstOpt = cType.firstOpt(this)
  // DEPRECATED: in favor of firstAs()
  def firstTyped[VT](elemT:PType[VT]):Option[VT] = if (isEmpty) None else Some(elemT.get(first))
  def firstAs[VT](elemT:PType[VT]):Option[VT] = {
    if (isEmpty)
      None
    else
      first.getOpt(elemT)
  }
  def wikify(context:QLContext, displayOpt:Option[Wikitext] = None):Wikitext = cType.doWikify(context)(cv, pType, displayOpt)
  
  def isEmpty = cType.isEmpty(this)
  def size = cv.size
  
  /**
   * Returns true iff these two QValues are equivalent.
   * 
   * TODO: this is probably subject to the same subclassing problems as == in Scala. Think it through
   * more carefully, and see if this logic makes sense in terms of subclassing.
   * 
   * TODO: this is current very strict in terms of matching cTypes, probably moreso than it needs to be.
   * Re-examine how we can loosen this while keeping it accurate.
   */
  def matches(other:QValue):Boolean = {
    if (other.cType != cType || other.pType != pType)
      false
    else if (cv.size != other.cv.size)
      false
    else {
      val pairs = cv.zip(other.cv)
      pairs.forall { pair =>
        pType.matches(pair._1, pair._2)
      }
    }
  }
  
  def debugRender(context:QLContext) = {
    cType.getClass().getSimpleName() + "[" + pType.getClass().getSimpleName() + "]" + "(" + cType.debugRender(context)(cv, pType) + ")"
  }
  
  // Returns the raw Iterable of ElemValues. Not often the right things to do, unless you
  // specifically don't care about type.
  def elems = cv
  
  /**********************
   * CASTING METHODS
   * 
   * These methods take a PType parameter mainly so that they can use the underlying VT. In
   * principle, we shouldn't be doing anything with that elemT parameter, but we *should*
   * be checking that it matches the actual PType of this QValue. (Using the same definition
   * of "matching" as in ElemValue.)
   * 
   * TODO: do that pType matches elemT check in all of these!
   **********************/
  // TODO: this isn't really flatMap, and shouldn't be named flatMap. Can we make things more
  // properly monadic?
  def flatMap[VT, T](elemT:PType[VT])(cb:VT => Option[T]) = cv.flatMap { elem => 
    val vt = elemT.get(elem)
    cb(vt)
  }
  
  /**
   * The primary transformer from one QValue to another. Takes a function that transforms the underlying
   * data types, and does the unwrapping, transforming of each element, and re-wrapping.
   */
  def map[VT, DT, RT](sourceType:PType[VT], destType:PType[DT] with PTypeBuilder[DT, RT])(cb:VT => RT):QValue = {
    val iter = cv.map { elem => 
      val vt = sourceType.get(elem)
      val result = cb(vt)
      destType(result)
    }
    cType.makePropValue(iter, destType)
  }
  
  def rawList[VT](elemT:PType[VT]):List[VT] = {
    (List.empty[VT] /: cv) ((list, elem) => list :+ elemT.get(elem))
  }
  
  def contains[VT](elemT:PType[VT], toCheck:VT):Boolean = cv.exists { elem =>
    val vt = elemT.get(elem)
    elemT.doMatches(vt, toCheck)
  }
  
  def exists[VT](elemT:PType[VT], check:VT => Boolean):Boolean = cv.exists { elem =>
    val vt = elemT.get(elem)
    check(vt)
  }
  
  def indexOf(toCheck:ElemValue):Option[Int] = {
    val pt = toCheck.pType
    if (pt != pType) {
      None
    } else {
      val index = cv.toList.indexWhere { elem =>
        pt.matches(elem, toCheck)
      }
      index match {
        case -1 => None
        case n:Int => Some(n)
      }
    }
  }
  
  def elemAt(index:Int):ElemValue = cv.toList(index)
}

object QValue {
  def make[DT, RT](cType:Collection, pType:PType[DT] with PTypeBuilder[DT, RT], vals:RT*):QValue = {
    val iter = vals.map(pType(_))
    cType.makePropValue(iter, pType)
  }
}

object ErrorTextType extends TextTypeBase(UnknownOID,
  Thing.toProps(
    Thing.setName("Error Text")
  )) with PTypeBuilder[QLText,String] {
}
object ExactlyOneCut extends ExactlyOne(UnknownOID) {
  override def makePropValue(cv:Iterable[ElemValue], elemT:PType[_]):QValue = new ExactlyOnePropValue(cv.toList, this, elemT) with CutProcessing
}
object EmptyListCut extends QList(UnknownOID) {
  def apply() = new QListPropValue(List.empty, this, UnknownType) with CutProcessing  
}

object ErrorValue {
  def apply(msg:String) = {
    try {
      throw new Exception("dummy")
    } catch {
      case e:Exception => Logger.error(s"Displaying error $msg; stack trace:\n${e.getStackTraceString}")  
    }
    WarningValue(msg)
  }
}
object TextValue {
  def apply(msg:String) = ExactlyOne(PlainTextType(msg))
}
object HtmlValue {
  def apply(html:Html):QValue = ExactlyOne(RawHtmlType(HtmlWikitext(html)))
  def apply(str:String):QValue = apply(Html(str))
}
object WikitextValue {
  def apply(wikitext:Wikitext):QValue = ExactlyOne(ParsedTextType(wikitext))
}
object LinkValue {
  def apply(target:OID) = ExactlyOne(LinkType(target))
}
object WarningValue {
  def apply(msg:String) = ExactlyOneCut(ErrorTextType("{{_warning:" + msg + "}}"))
}
object EmptyValue {
  // TODO: do something with this?
  def apply(pType:PType[_]) = QList.empty
  // TODO: do we need this?
  def untyped = QList.empty
}
