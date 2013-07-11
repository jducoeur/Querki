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

trait TypedValue {
  // All of these are actually defined in PropValue:
  def ct:Collection
  def pt:PType[_]
  def render(context:ContextBase):Wikitext
  def firstAs[VT](elemT:PType[VT]):Option[VT]
  
  // Obsolete leftover from the old TypedValue -- was a wrapper around PropValue
  def v = this.asInstanceOf[PropValue]
  
  // We are cutting iff the constructor mixed in CutProcessing:
  def cut = this.isInstanceOf[CutProcessing]
}

object ErrorTextType extends TextTypeBase(UnknownOID,
  Thing.toProps(
    Thing.setName("Error Text")
  )) with PTypeBuilder[QLText,String] {
}
object ExactlyOneCut extends ExactlyOne(UnknownOID) {
  override def makePropValue(cv:implType, elemT:PType[_]):PropValue = new ExactlyOnePropValue(cv, this, elemT) with CutProcessing
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
  def apply(wikitext:Wikitext) = ExactlyOne(ParsedTextType(wikitext))
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
