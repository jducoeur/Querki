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
    
    override def validate(v:String):Boolean = try {
      val dummy = java.lang.Integer.parseInt(v)
      true
    } catch {
      case _ => false      
    }


    val doDefault = 0
  }
  object IntType extends IntType(IntTypeOID)

  abstract class TextTypeBase(oid:OID, pf:PropFetcher) extends SystemType[Wikitext](oid, pf
      ) with PTypeBuilder[Wikitext,String]
  {
    // TODO: escape JSON special chars for serialization!
    
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
    def doDeserialize(ser:String) = java.lang.Boolean.parseBoolean(ser)
    def doSerialize(v:Boolean) = v.toString
    def doRender(v:Boolean) = Wikitext(v.toString())
    
    override def validate(v:String):Boolean = try {
      java.lang.Boolean.parseBoolean(v)
    } catch {
      case _ => false
    }
    
    val doDefault = false
  }
  object YesNoType extends YesNoType(YesNoTypeOID)
  
  /**
   * The Type for Display Names -- similar to Text, but not identical
   * 
   * TODO: introduce validation, since only a subset of chars are legal (I think)
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
    
    /**
     * Names are only allowed to contain alphanumerics, dashes and spaces
     */
    override def validate(v:String):Boolean = {
      (true /: v)((current, c) => current && c.isLetterOrDigit || c == '-' || c == ' ')
    }
    
    val doDefault = "MISSING NAME!"
  }
  object NameType extends NameType(NameTypeOID)
  
  /**
   * The Type for Links to other Things
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
    
    // TODO: define validate()

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
  // TODO: filter any Javascript-enabling keywords! This should go in validate, as well
  // as fromUser().
    
  def doDeserialize(v:String) = v
  def doSerialize(v:String) = v
  def doRender(v:String) = Wikitext(v)
    
  val doDefault = ""
}
object CSSTextType extends CSSTextType(CSSTextOID)
  
  
object SystemTypes {
  def all = Space.oidMap[PType[_]](IntType, TextType, YesNoType, NameType, LinkType, LargeTextType, CSSTextType)  
}