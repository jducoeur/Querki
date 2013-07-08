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

// TODO: we've gotten rid of the explicit ct parameter, since it is contained in v.
// Maybe we can do the same for pt?
case class TypedValue(v:PropValue, pt:PType[_], cut:Boolean = false) {
  def ct:Collection = v.coll
  
  def render(context:ContextBase):Wikitext = v.render(context, pt) 
  
  def firstTyped[VT](expectedType:PType[VT]):Option[VT] = {
    if (expectedType == pt) {
      v.firstTyped(expectedType)
    } else
      None
  }
}
object ErrorValue {
  def apply(msg:String) = {
    try {
      throw new Exception("dummy")
    } catch {
      case e:Exception => Logger.error(s"Displaying error $msg; stack trace:\n${e.getStackTraceString}")  
    }
    TypedValue(ExactlyOne(PlainTextType(msg)), PlainTextType, true)
  }
}
object TextValue {
  def apply(msg:String) = TypedValue(ExactlyOne(PlainTextType(msg)), PlainTextType)
}
object HtmlValue {
  def apply(html:Html) = TypedValue(ExactlyOne(RawHtmlType(HtmlWikitext(html))), RawHtmlType)
}
object WikitextValue {
  def apply(wikitext:Wikitext) = TypedValue(ExactlyOne(ParsedTextType(wikitext)), ParsedTextType)
}
object LinkValue {
  def apply(target:OID) = TypedValue(ExactlyOne(LinkType(target)), LinkType)
}
object WarningValue {
  def apply(msg:String) = TypedValue(ExactlyOne(TextType("{{_warning:" + msg + "}}")), TextType, true)
}
object EmptyValue {
  def apply(pType:PType[_]) = TypedValue(QList.empty, pType)
  // TODO: this is evil -- in the long run, we should eliminate this and think more carefully about what
  // the correct type is in cases where it is being used:
  def untyped = TypedValue(QList.empty, YesNoType)
}
