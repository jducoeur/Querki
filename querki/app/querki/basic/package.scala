package querki

import models.{Property, PType, PTypeBuilder, Thing}

import querki.ecology._

import querki.core.QLText

import querki.values.QValue

/**
 * This Module contains "Basic" Querki -- some Properties and such that aren't core (not central to the
 * operations of the system), but which all users will probably want to make use of.
 */
package object basic {
  object MOIDs extends EcotIds(17) {
    val DisplayTextOID = sysId(7)
    val PageOID = sysId(8)
    val SimpleThingOID = sysId(23)
    val DisplayNameOID = sysId(26)
    val PhotoBaseOID = sysId(30)
    val PlainTextOID = sysId(37)
    val QLTypeOID = sysId(45)
    val ApplyMethodOID = sysId(46)
    val BulletedOID = sysId(56)
    val DisplayThingTreeOID = sysId(66)
    val AllThingsOID = sysId(67)
    val CommasMethodOID = sysId(80)
    val AllPropsThingOID = sysId(84)
    val DeprecatedOID = sysId(101)
    
    val ExplicitPropOID = moid(1)
    val SystemOnlyPropOID = moid(2)
  }
  
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
  
  trait Basic extends EcologyInterface {  
    def PlainTextType:PType[PlainText] with PTypeBuilder[PlainText, String]
    def QLType:PType[QLText] with PTypeBuilder[QLText,String]
    
    def ApplyMethod:Property[QLText,String]    
    def DeprecatedProp:Property[Boolean,Boolean]
    def DisplayNameProp:Property[PlainText,String]
    def DisplayTextProp:Property[QLText,String]
    def ExplicitProp:Property[Boolean,Boolean]
    def SystemOnlyProp:Property[Boolean,Boolean]
    
    def Page:Thing
    def SimpleThing:Thing
    
    def TextValue(msg:String):QValue
  }
}