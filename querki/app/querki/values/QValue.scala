package querki.values

import models._
import models.system._

// TODO: both of these should get evolved away!
import play.api.Logger
import play.api.templates.Html

import querki.values._

/**
 * This is a fake PType, which exists so that we can persist embedded Texts in the pipeline.
 */
object ParsedTextType extends SystemType[Wikitext](OIDs.IllegalOID, () => Thing.emptyProps) with SimplePTypeBuilder[Wikitext]
{
  def doDeserialize(v:String) = throw new Exception("Can't deserialize ParsedText!")
  def doSerialize(v:Wikitext) = throw new Exception("Can't serialize ParsedText!")
  def doRender(context:ContextBase)(v:Wikitext) = v
  
  override def doDebugRender(context:ContextBase)(v:Wikitext) = v.contents.map(_.internal).mkString
  
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
  def doRender(context:ContextBase)(v:Wikitext) = v
    
  val doDefault = Wikitext("")
}

/**
 * This is a fake PType, used when we encounter a name we don't know.
 */
object UnknownNameType extends NameType(UnknownOID, "_unknownNameType") {
  def doRender(context:ContextBase)(v:String) = Wikitext("{{_unknownName:") + nameToLink(context)(v) + Wikitext("}}")
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
  def first = cType.first(this)
  // DEPRECATED: in favor of firstAs()
  def firstTyped[VT](elemT:PType[VT]):Option[VT] = if (isEmpty) None else Some(elemT.get(first))
  def firstAs[VT](elemT:PType[VT]):Option[VT] = {
    if (isEmpty)
      None
    else
      first.getOpt(elemT)
  }
  def render(context:ContextBase):Wikitext = cType.doRender(context)(cv, pType)
  
  def isEmpty = cType.isEmpty(this)
  def size = cv.size
  
  def debugRender(context:ContextBase) = {
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
  def flatMap[VT, T](elemT:PType[VT])(cb:VT => Option[T]) = cv.flatMap { elem => 
    val vt = elemT.get(elem)
    cb(vt)
  }
  
  def rawList[VT](elemT:PType[VT]):List[VT] = {
    (List.empty[VT] /: cv) ((list, elem) => list :+ elemT.get(elem))
  }
  
  def contains[VT](elemT:PType[VT], toCheck:VT):Boolean = cv.exists { elem =>
    val vt = elemT.get(elem)
    elemT.matches(vt, toCheck)
  }
}

object ErrorTextType extends TextTypeBase(UnknownOID,
  Thing.toProps(
    Thing.setName("Error Text")
  )) with PTypeBuilder[QLText,String] {
}
object ExactlyOneCut extends ExactlyOne(UnknownOID) {
  override def makePropValue(cv:implType, elemT:PType[_]):QValue = new ExactlyOnePropValue(cv, this, elemT) with CutProcessing
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
  def apply(html:Html) = ExactlyOne(RawHtmlType(HtmlWikitext(html)))
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
