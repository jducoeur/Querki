package models.system

import play.api.Logger
import play.api.templates.Html

import models._

import Thing._

import OIDs._

import ql._

abstract class SystemType[T](tid:OID, pf:PropFetcher) extends PType[T](tid, systemOID, RootOID, pf)

object CommonInputRenderers {
  def renderLargeText(prop:Property[_, _,_], state:SpaceState, currentValue:Option[String]):Html = {
    val v = currentValue getOrElse ""
    val xml = 
      <textarea rows="5" cols="50" name={"v-" + prop.id.toString} placeholder={prop.getProp(PlaceholderTextProp)(state).renderPlainIfDefined.raw}>{v}</textarea>
    Html(xml.toString)
  }
  
  def renderText(prop:Property[_, _,_], state:SpaceState, currentValue:Option[String]):Html = {
    val v = currentValue getOrElse ""
    val xml = 
      <input type="text" name={"v-" + prop.id.toString} value={v} placeholder={prop.getProp(PlaceholderTextProp)(state).renderPlainIfDefined.raw}/>
    Html(xml.toString)
  }

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
      ) with PTypeBuilder[QLText,String]
  {
    def doDeserialize(v:String) = QLText(v)
    def doSerialize(v:QLText) = v.text
    def doRender(context:ContextBase)(v:QLText) = {
      val parser = new QLParser(v, context)
      parser.process
    }
    
    val doDefault = QLText("")
    def wrap(raw:String):valType = QLText(raw)
    
    override def renderInput(prop:Property[_,_,_], state:SpaceState, currentValue:Option[String]):Html = 
      CommonInputRenderers.renderText(prop, state, currentValue)
  }

  /**
   * The Type for Text -- probably the most common type in Querki
   */
  class TextType(tid:OID) extends TextTypeBase(tid,
      toProps(
        setName("Type-Text")
        )) with PTypeBuilder[QLText,String] {
    override def renderInput(prop:Property[_,_,_], state:SpaceState, currentValue:Option[String]):Html = 
      CommonInputRenderers.renderText(prop, state, currentValue)
  }
  object TextType extends TextType(TextTypeOID)
  
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
        
        case _ => throw new Exception("I can't interpret " + v + " as a YesNo value")
      }
    }
    def doSerialize(v:Boolean) = v.toString
    def doRender(context:ContextBase)(v:Boolean) = Wikitext(v.toString())
    
    val doDefault = false
    
    override def renderInput(prop:Property[_,_,_], state:SpaceState, currentValue:Option[String]):Html = {
      val xml = <input type="checkbox" name={"v-" + prop.id.toString}/>
      Html(xml.toString)
    }
  }
  object YesNoType extends YesNoType(YesNoTypeOID)
  
  /**
   * The Type for Display Names -- similar to Text, but not identical
   */
  class NameType(tid:OID) extends SystemType[String](tid,
      toProps(
        setName("Type-Name")
        )) with SimplePTypeBuilder[String]
  {
    def toInternal(str:String) = str.replaceAll(" ", "-")
    def toDisplay(str:String) = str.replaceAll("-", " ")
        
    def doDeserialize(v:String) = toDisplay(v)
    def doSerialize(v:String) = toInternal(v)
    def doRender(context:ContextBase)(v:String) = Wikitext(toDisplay(v))
    
    override protected def doToUser(v:String):String = toDisplay(v)
    override protected def doFromUser(v:String):String = {
      if (v.length() == 0)
        throw new Exception("Names must have non-zero length")
      else if (v.exists(c => !c.isLetterOrDigit && c != '-' && c != ' '))
        throw new Exception("Names may only contain letters, digits, dashes and spaces")
      else
        toDisplay(v)
    }
    
    def equalNames(str1:String, str2:String):Boolean = {
      canonicalize(str1).contentEquals(canonicalize(str2))
    }
    
    def canonicalize(str:String):String = toInternal(str).toLowerCase

    val doDefault = "MISSING NAME!"
      
    override def renderInput(prop:Property[_,_,_], state:SpaceState, currentValue:Option[String]):Html = 
      CommonInputRenderers.renderText(prop, state, currentValue)
  }
  object NameType extends NameType(NameTypeOID)
  
  /**
   * The Type for Links to other Things
   * 
   * TODO: This Type, and its associated Properties, may want to become a Module.
   */
  class LinkType(tid:OID) extends SystemType[OID](tid,
      toProps(
        setName("Type-Link")
        )) with SimplePTypeBuilder[OID]
  {
    def doDeserialize(v:String) = OID(v)
    def doSerialize(v:OID) = v.toString

    def doRender(context:ContextBase)(v:OID) = {
      val target = context.state.anything(v)
      val text = target match {
        case Some(t) => "[" + t.displayName + "](" + t.toThingId + ")"
        case None => "Bad Link: Thing " + v.toString + " not found"
      }
      Wikitext(text)
    }
    
    // TODO: define doFromUser()

    val doDefault = UnknownOID
    
    override def renderInput(prop:Property[_,_,_], state:SpaceState, v:Option[String]):Html = {
      val result = 
        <select name={ "v-" + prop.id.toString }> {
          val candidates = state.linkCandidates(prop)
          candidates map { candidate =>
            if(v.isDefined && (candidate.id.toString == v.get)) {
              <option value={candidate.id.toString} selected="selected">{candidate.displayName}</option>        
            } else {
              <option value={candidate.id.toString}>{candidate.displayName}</option>
            }
          }
        } </select>
      Html(result.toString)
    }
  }
  object LinkType extends LinkType(LinkTypeOID)

  /**
   * The Type for Large Text -- stuff that we expect to take up more space on-screen
   */
  class LargeTextType(tid:OID) extends TextTypeBase(tid,
      toProps(
        setName("Type-Large-Text")
        )) with PTypeBuilder[QLText,String] {
    override def renderInput(prop:Property[_,_,_], state:SpaceState, currentValue:Option[String]):Html =
      CommonInputRenderers.renderLargeText(prop, state, currentValue)
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
  def wrap(raw:String):valType = PlainText(raw)
    
  override def renderInput(prop:Property[_,_,_], state:SpaceState, currentValue:Option[String]):Html = 
    CommonInputRenderers.renderText(prop, state, currentValue)
}
object PlainTextType extends PlainTextType(PlainTextOID)

object SystemTypes {
  def all = Space.oidMap[PType[_]](IntType, TextType, YesNoType, NameType, LinkType, LargeTextType, PlainTextType)  
}