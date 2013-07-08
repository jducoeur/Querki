package models.system

import scala.xml._

import play.api.Logger
import play.api.templates.Html

import models._

import Thing._

import OIDs._

import ql._

import querki.values._

abstract class SystemType[T](tid:OID, pf:PropFetcher) extends PType[T](tid, systemOID, RootOID, pf) {
  def renderInputXml(prop:Property[_,_], state:SpaceState, currentValue:DisplayPropVal, v:ElemValue):Elem = 
    CommonInputRenderers.renderText(prop, state, currentValue, v, this)
}

object CommonInputRenderers {
  def renderAnyText(prop:Property[_, _], state:SpaceState, currentValue:DisplayPropVal, v:ElemValue, elemT:PType[_])(doRender: (String) => Elem):Elem = {
    val str = elemT.toUser(v)
    val xml = doRender(str)
    // TODO: how should placeholder work in the new world? The placeholder should happen at the
    // collection-input level? Or am I mis-thinking that? There is only one text-input field, even
    // for a list, after all.
//    val placeholder:String =
//      if (currentValue.hasInheritance)
//        currentValue.inheritedVal.get
//      else
//        prop.getProp(PlaceholderTextProp)(state).renderPlainIfDefined.raw
//    val xml2 = xml % Attribute("placeholder", Text(placeholder), Null)
//    xml2
    xml
  }
  
  def renderLargeText(prop:Property[_, _], state:SpaceState, currentValue:DisplayPropVal, v:ElemValue, elemT:PType[_]):Elem = {
    renderAnyText(prop, state, currentValue, v, elemT) { cv =>
      <textarea class="_largeTextEdit" rows="2">{cv}</textarea>
    }
  }
  
  def renderText(prop:Property[_, _], state:SpaceState, currentValue:DisplayPropVal, v:ElemValue, elemT:PType[_]):Elem = {
    renderAnyText(prop, state, currentValue, v, elemT) { cv =>
      <input type="text" value={cv}/>
    }
  }
}

/**
 * This trait should be used by any type that can consider itself "code" -- in particular, that wants to be
 * displayable in the _code() method. 
 */
trait CodeType {
  def code(elem:ElemValue):String
}

  /**
   * The Type for integers
   */
  class IntType(tid:OID) extends SystemType[Int](tid,
      toProps(
        setName("Type-Whole-Number")
        )) with SimplePTypeBuilder[Int]
  {
    def doDeserialize(v:String) = java.lang.Integer.parseInt(v)
    def doSerialize(v:Int) = v.toString
    def doRender(context:ContextBase)(v:Int) = Wikitext(v.toString)

    val doDefault = 0
    
    /**
     * TODO: eventually, we may want a more nuanced Int inputter. But this will do to start.
     */
  }
  object IntType extends IntType(IntTypeOID)
  
  /**
   * QLText is a String that may contain both Wikitext and QL expressions. It must go through two
   * transformations before display:
   * 
   * -- Processing, which parses and computes the QL expressions, turning them into Wikitext.
   * -- Rendering, which turns the Wikitext into the final output format. (Usually HTML.)
   * 
   * Processing always happens in the server; rendering happens at the client for the typical
   * web-browser UI, or in the client if you have a smart client (eg, a smartphone app).
   * 
   * QLText mainly exists for security purposes: the pipeline of QLText -> Wikitext -> Html
   * doesn't necessarily do any transformation at all, but reifies the semantics of what's
   * allowed and what needs processing before display. This is mainly to ensure that, eg,
   * raw HTML doesn't get through when it's not allowed.
   */
  case class QLText(text:String)
  
  abstract class TextTypeBase(oid:OID, pf:PropFetcher) extends SystemType[QLText](oid, pf
      ) with PTypeBuilder[QLText,String] with CodeType
  {
    def doDeserialize(v:String) = QLText(v)
    def doSerialize(v:QLText) = v.text
    def doRender(context:ContextBase)(v:QLText) = {
      val parser = new QLParser(v, context)
      parser.process
    }
    
    val doDefault = QLText("")
    def wrap(raw:String):valType = QLText(raw)
    
    def code(elem:ElemValue):String = get(elem).text
  }

  /**
   * The Type for Text -- probably the most common type in Querki
   */
  class TextType(tid:OID) extends TextTypeBase(tid,
      toProps(
        setName("Type-Text")
        )) with PTypeBuilder[QLText,String] {
  }
  object TextType extends TextType(TextTypeOID)

/**
 * A QL field is sort of like inside-out QLText. It is processed very similarly,
 * but whereas the "outer" layer of QLText is expected to be QText, with QL in
 * subclauses, the outer layer of a QL field is QL, with wikitext in subclauses.
 * 
 * In other words, it is like QLText, but just the stuff inside the [[ ]] parts.
 * 
 * QL fields are also processed a bit differently. QLText is fully processed and
 * rendered, producing QText. QL fields are essentially methods, which get *called*
 * from other methods and from QLText. So the results are not turned directly into
 * QText; instead, the resulting Context is fed back out to the caller.
 */
class QLType(tid:OID) extends TextTypeBase(tid,
    toProps(
      setName("Type QL")
    )) with PTypeBuilder[QLText,String] 
{
  override def renderInputXml(prop:Property[_,_], state:SpaceState, currentValue:DisplayPropVal, v:ElemValue):Elem =
    CommonInputRenderers.renderLargeText(prop, state, currentValue, v, this)

  // TBD: in principle, we really want this to return a *context*, not a *value*. This is a special
  // case of a growing concern: that we could be losing information by returning TypedValue from
  // qlApply, and should actually be returning a full successor Context.
  override def qlApplyFromProp(definingContext:ContextBase, incomingContext:ContextBase, prop:Property[QLText,_], params:Option[Seq[QLPhrase]]):Option[TypedValue] = {
    if (definingContext.isEmpty) {
      Some(ErrorValue("""Trying to use QL Property """" + prop.displayName + """" in an empty context.
This often means that you've invoked it recursively without saying which Thing it is defined in."""))
    } else {
      Some(prop.applyToIncomingThing(definingContext) { (thing, context) =>
        val qlPhraseText = thing.first(prop)(context.state)
        val parser = new QLParser(qlPhraseText, incomingContext.forProperty(prop))
        parser.processMethod.value
      })
    }
  }
}
object QLType extends QLType(QLTypeOID)
  
  /**
   * The YesNo Type -- or Boolean, as us geeks think of it
   */
  class YesNoType(tid:OID) extends SystemType[Boolean](tid,
      toProps(
        setName("Type-YesNo")
        )) with SimplePTypeBuilder[Boolean]
  {
    // It turns out that Java's parseBoolean is both too tolerant of nonsense, and
    // doesn't handle many common cases. So we'll do it ourselves:
    def doDeserialize(v:String) = {
      v.toLowerCase() match {
        case "true" => true
        case "false" => false
        
        case "1" => true
        case "0" => false
        
        case "yes" => true
        case "no" => false
        
        case "on" => true
        case "off" => false
        
        case "t" => true
        case "f" => false
        
        case "y" => true
        case "n" => false
        
        // Okay, this one looks odd, but it relates to the way checkboxes get handled in HTTP. If the checkbox
        // is empty (false), then it *is not transmitted*! If it is checked (true), then it gets transmitted,
        // but its value is sometimes left empty; the fact that it is sent at all means that it is true.
        // TBD: this is kind of idiotic. Why is it inconsistently happening?
        case "" => true
        
        case _ => throw new Exception("I can't interpret " + v + " as a YesNo value")
      }
    }
    def doSerialize(v:Boolean) = v.toString
    def doRender(context:ContextBase)(v:Boolean) = Wikitext(v.toString())
    
    val doDefault = false
    
    override def renderInputXml(prop:Property[_,_], state:SpaceState, currentValue:DisplayPropVal, v:ElemValue):Elem = {
      if (get(v))
        <input type="checkbox" checked="checked" />
      else
        <input type="checkbox"/>
    }
  }
  object YesNoType extends YesNoType(YesNoTypeOID) {
    val True = YesNoType(true)
    val False = YesNoType(false)
    
    implicit def boolean2YesNo(raw:Boolean):ElemValue = {
      if (raw)
        YesNoType.True
      else
        YesNoType.False
    }
    
    implicit def boolean2YesNoTypedValue(raw:Boolean):TypedValue = {
      TypedValue(ExactlyOne(raw), YesNoType)
    }
    
    def toBoolean(typed:TypedValue):Boolean = {
      typed match {
        case TypedValue(propVal, YesNoType, _) => propVal.firstTyped(YesNoType).getOrElse(false)
        case _ => false
      }
    }
  }
  
  trait NameableType {
    def getName(context:ContextBase)(v:ElemValue):String
  }
  
  /**
   * The Type for Display Names -- similar to Text, but not identical
   */
  abstract class NameType(tid:OID, name:String) extends SystemType[String](tid,
      toProps(
        setName(name)
        )) with SimplePTypeBuilder[String] with NameableType
  {
    def toInternal(str:String) = str.replaceAll(" ", "-")
    def toDisplay(str:String) = str.replaceAll("-", " ")
    // Note that this deliberately allows mixed-case, so that we can preserve Name case through
    // the URL for Tags. (Since the desired case *only* exists in the URL.)
    def toUrl = toInternal _
        
    def doDeserialize(v:String) = toDisplay(v)
    def doSerialize(v:String) = toInternal(v)
    
    override def doToUser(v:String):String = toDisplay(v)
    override protected def doFromUser(v:String):String = {
      if (v.length() == 0)
        throw new Exception("Names must have non-zero length")
      else if (v.exists(c => !c.isLetterOrDigit && c != '-' && c != ' ' && c != '/'))
        throw new Exception("Names may only contain letters, digits, dashes and spaces")
      else
        toDisplay(v)
    }
    
    def equalNames(str1:String, str2:String):Boolean = {
      canonicalize(str1).contentEquals(canonicalize(str2))
    }
    
    def canonicalize(str:String):String = toInternal(str).toLowerCase
    
    def getName(context:ContextBase)(v:ElemValue) = canonicalize(get(v))
    
    def nameToLink(context:ContextBase)(v:String) = {
      // Conceptually, toInternal isn't quite right here. We're using it instead of AsName
      // mostly because we want to preserve the *case* of the Tag:
      Wikitext("[" + v + "](" + toUrl(v) + ")")
    }
    
    override def doComp(context:ContextBase)(left:String, right:String):Boolean = { left < right } 

    val doDefault = ""
      
    override def matches(left:String, right:String):Boolean = equalNames(left, right)
  }
  object NameType extends NameType(NameTypeOID, "Type-Name") {
    def doRender(context:ContextBase)(v:String) = Wikitext(toDisplay(v))    
  }
  object TagSetType extends NameType(TagSetOID, "Tag Set") {
    override def requiredColl:Option[Collection] = Some(QSet)
    
    // TODO: this should probably get refactored with LinkType? They're different ways of
    // expressing the same concepts; it's just that Links are OID-based, whereas Names/Tags are
    // name-based.
    def doRender(context:ContextBase)(v:String) = nameToLink(context)(v)
    
    override def renderProperty(prop:Property[_,_])(implicit request:controllers.RequestContext):Option[Wikitext] = {
      val parser = new ql.QLParser(QLText("""These tags are currently being used:
[[_tagsForProperty -> _sort -> _bulleted]]"""), prop.thisAsContext)
      Some(parser.process)
    }
  }
  
  /**
   * The Type for Links to other Things
   * 
   * TODO: This Type, and its associated Properties, may want to become a Module.
   */
  class LinkType(tid:OID) extends SystemType[OID](tid,
      toProps(
        setName("Type-Link")
        )) with SimplePTypeBuilder[OID] with NameableType
  {
    def doDeserialize(v:String) = OID(v)
    def doSerialize(v:OID) = v.toString
    
    def follow(context:ContextBase)(v:OID) = context.state.anything(v)
    def followLink(context:ContextBase):Option[Thing] = {
      // This should only be called if the valType is LinkType, and the Collection is
      // single-valued!
      // TODO: Evil! ElemValue really ought to have its own mapping into the PType.
      val oid = context.value.v.first.elem.asInstanceOf[OID]
      follow(context)(oid)
    }

    def doRender(context:ContextBase)(v:OID) = {
      val target = follow(context)(v)
      val text = target match {
        case Some(t) => "[" + t.displayName + "](" + t.toThingId + ")"
        case None => "Bad Link: Thing " + v.toString + " not found"
      }
      Wikitext(text)
    }
    
    def getNameFromId(context:ContextBase)(id:OID) = {
      val tOpt = follow(context)(id)
      tOpt.map(thing => NameType.canonicalize(thing.displayName)).getOrElse(throw new Exception("Trying to get name from unknown OID " + id))      
    }
    def getName(context:ContextBase)(v:ElemValue) = {
      val id = get(v)
      getNameFromId(context)(id)
    }
    
    // Links are sorted by their *display names*:
    override def doComp(context:ContextBase)(left:OID, right:OID):Boolean = { 
      NameType.doComp(context)(getNameFromId(context)(left), getNameFromId(context)(right))
    } 
    
    // TODO: define doFromUser()

    val doDefault = UnknownOID
    
    def renderInputXmlGuts(prop:Property[_,_], state:SpaceState, currentValue:DisplayPropVal, v:ElemValue):Iterable[Elem] = {
      val candidates = state.linkCandidates(prop).toSeq.sortBy(_.displayName)
      val realOptions =
        if (candidates.isEmpty) {
          Seq(<option value={UnknownOID.toString}><i>None defined</i></option>)
        } else {
          candidates map { candidate:Thing =>
            if(candidate.id == v.elem) {
              <option value={candidate.id.toString} selected="selected">{candidate.displayName}</option>        
            } else {
              <option value={candidate.id.toString}>{candidate.displayName}</option>
            }
          }
        }
      val linkModel = prop.getPropOpt(LinkModelProp)(state)
      linkModel match {
        case Some(propAndVal) => {
          val model = state.anything(propAndVal.first).get
          realOptions :+ <option class="_createNewFromModel" data-model={model.toThingId} value={UnknownOID.id.toString}>Create a New {model.displayName}</option>
        }
        case _ => realOptions
      }
    }
    override def renderInputXml(prop:Property[_,_], state:SpaceState, currentValue:DisplayPropVal, v:ElemValue):Elem = {
        <select class="_linkSelect"> {
          renderInputXmlGuts(prop, state, currentValue, v)
        } </select>
    }
  }
  object LinkType extends LinkType(LinkTypeOID)
  object LinkFromThingBuilder extends PTypeBuilder[OID, Thing] {
    def wrap(raw:Thing):OID = raw.id
  }

  /**
   * The Type for Large Text -- stuff that we expect to take up more space on-screen
   */
  class LargeTextType(tid:OID) extends TextTypeBase(tid,
      toProps(
        setName("Type-Large-Text")
        )) with PTypeBuilder[QLText,String] {
    override def renderInputXml(prop:Property[_,_], state:SpaceState, currentValue:DisplayPropVal, v:ElemValue):Elem =
      CommonInputRenderers.renderLargeText(prop, state, currentValue, v, this)
  }
  object LargeTextType extends LargeTextType(LargeTextTypeOID)
  
/**
 * PlainText is essentially a simple String -- it represents a String field that does *not* contain
 * QL or Wikitext. It is used for a few Properties like Display Name, that are more flexible than NameType
 * but still can't go hog-wild.
 * 
 * Note that, while PlainText is mostly rendered literally, it still has to be HTML-neutered before display.
 */
case class PlainText(text:String) {
  def raw:String = {
    text.replaceAll("&", "&amp;").replaceAll("<", "&lt;")
  }
}
  
abstract class PlainTextType(tid:OID) extends SystemType[PlainText](tid,
    toProps(
      setName("Plain-Text")
    )) with PTypeBuilder[PlainText,String]
{
  def doDeserialize(v:String) = PlainText(v)
  def doSerialize(v:PlainText) = v.text
  // TODO: this is probably incorrect, but may be taken care of by context? How do we make sure this
  // doesn't actually get any internal Wikitext rendered?
  def doRender(context:ContextBase)(v:PlainText) = Wikitext(v.text)
    
  val doDefault = PlainText("")
  override def wrap(raw:String):valType = PlainText(raw)
}
object PlainTextType extends PlainTextType(PlainTextOID)

class InternalMethodType(tid:OID) extends SystemType[String](tid,
    toProps(
      setName("Internal Method")
    )) with SimplePTypeBuilder[String]
{
  def boom = throw new Exception("InternalMethodType cannot be used conventionally. It simply wraps code.")
  def doDeserialize(v:String) = boom
  def doSerialize(v:String) = boom

  def doRender(context:ContextBase)(v:String) = Wikitext("Internal Method")
    
  val doDefault = ""
  override def wrap(raw:String):valType = boom 
}
object InternalMethodType extends InternalMethodType(InternalMethodOID)

import java.net.URL
class ExternalLinkType(tid:OID) extends SystemType[URL](tid,
    toProps(
      setName("URL")
    )) with PTypeBuilder[URL, String]
{
  def doDeserialize(v:String) = new URL(v)
  def doSerialize(v:URL) = v.toExternalForm()
  def doRender(context:ContextBase)(v:URL) = Wikitext("[" + v.toExternalForm() + "](" + v.toExternalForm() + ")")
  
  val doDefault = new URL("http:///")
  override def wrap(raw:String):valType = new URL(raw)
}
object ExternalLinkType extends ExternalLinkType(ExternalLinkTypeOID)

// This is a pure marker trait, indicating that this PropValue didn't load correctly yet:
trait UnresolvedPropValue
object UnresolvedProp extends ExactlyOne(UnknownOID) {
   override def makePropValue(cv:implType):PropValue = UnresPropValue(cv, this)
   private case class UnresPropValue(cv:implType, coll:ExactlyOne) extends PropValue with UnresolvedPropValue
}
// This pseudo-Type is used to store values from disk that we can't resolve yet. It is only
// used at Space-load time:
object UnresolvedPropType extends SystemType[String](UnknownOID,
    toProps(
      setName("UnresolvedProp")
    )) with SimplePTypeBuilder[String]
{
  def doDeserialize(v:String) = v
  def doSerialize(v:String) = v
  def doRender(context:ContextBase)(v:String) = Wikitext("Unresolved property value!")
  
  val doDefault = ""
}

object SystemTypes {
  def all = Space.oidMap[PType[_]](IntType, TextType, QLType, YesNoType, NameType, TagSetType, LinkType, LargeTextType, PlainTextType, InternalMethodType, ExternalLinkType)  
}