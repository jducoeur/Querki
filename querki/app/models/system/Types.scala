package models.system

import models._

import Thing._

import OIDs._

abstract class SystemType[T](tid:OID, pf:PropFetcher) extends PType[T](tid, systemOID, RootOID, pf)

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
    def doRender(v:Int) = Wikitext(v.toString)

    val doDefault = 0
  }
  object IntType extends IntType(IntTypeOID)

  abstract class TextTypeBase(oid:OID, pf:PropFetcher) extends SystemType[Wikitext](oid, pf
      ) with PTypeBuilder[Wikitext,String]
  {
    def doDeserialize(v:String) = Wikitext(v)
    def doSerialize(v:Wikitext) = v.internal
    def doRender(v:Wikitext) = v
    
    val doDefault = Wikitext("")
    def wrap(raw:String):valType = Wikitext(raw)
  }
  
  /**
   * The Type for Text -- probably the most common type in Querki
   */
  class TextType(tid:OID) extends TextTypeBase(tid,
      toProps(
        setName("Type-Text")
        )) with PTypeBuilder[Wikitext,String]
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
    def doRender(v:Boolean) = Wikitext(v.toString())
    
    val doDefault = false
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
    def doRender(v:String) = Wikitext(toDisplay(v))
    
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
  }
  object NameType extends NameType(NameTypeOID)
  
  /**
   * The Type for Links to other Things
   * 
   * TODO: this wants to become Typed Links very, very soon. Most use cases don't want me to
   * list every Thing in the Space.
   */
  class LinkType(tid:OID) extends SystemType[OID](tid,
      toProps(
        setName("Type-Link")
        )) with SimplePTypeBuilder[OID]
  {
    def doDeserialize(v:String) = OID(v)
    def doSerialize(v:OID) = v.toString
    // TODO: this is a good illustration of the fact that render should actually
    // be contextual -- you can't really render a Link in isolation, without knowing
    // about the Thing it points to:
    def doRender(v:OID) = Wikitext(v.toString)
    
    // TODO: define doFromUser()

    val doDefault = UnknownOID
  }
  object LinkType extends LinkType(LinkTypeOID)

  /**
   * The Type for Large Text -- stuff that we expect to take up more space on-screen
   */
  class LargeTextType(tid:OID) extends TextTypeBase(tid,
      toProps(
        setName("Type-Large-Text")
        )) with PTypeBuilder[Wikitext,String]
  object LargeTextType extends LargeTextType(LargeTextTypeOID)
  
/**
 * A Type for CSS Text as a proper Property, so we can edit directly in Querki.
 */
class CSSTextType(tid:OID) extends SystemType[String](tid,
    toProps(
        setName("Type-CSS"))
    ) with SimplePTypeBuilder[String]
{
  // TODO: filter any Javascript-enabling keywords! This should go in doFromUser().
    
  def doDeserialize(v:String) = v
  def doSerialize(v:String) = v
  def doRender(v:String) = Wikitext(v)
    
  val doDefault = ""
}
object CSSTextType extends CSSTextType(CSSTextOID)
  
  
object SystemTypes {
  def all = Space.oidMap[PType[_]](IntType, TextType, YesNoType, NameType, LinkType, LargeTextType, CSSTextType)  
}