package querki

import collection.immutable.TreeMap

import models.{Collection, DisplayPropVal, OID, Property, PType, PTypeBuilder, PTypeBuilderBase, Thing}

import querki.ecology._

import querki.values.{ElemValue, QLContext, QValue, SpaceState}

/**
 * Querki's "core" objects
 * 
 * This is conceptually the center of the onion, at least in terms of static declarations of
 * actual Things. querki.core contains the Things that are *frequently* used elsewhere. As such,
 * it can be depended upon by all other Modules, and doesn't depend on any of them.
 */
package object core {
  object MOIDs extends EcotIds(16) {
    val RootOID = sysId(1)
    val IntTypeOID = sysId(2)
    val TextTypeOID = sysId(3)
    val YesNoTypeOID = sysId(4)
    val UrPropOID = sysId(5)
    val NameOID = sysId(6)
    val NameTypeOID = sysId(10)
    val UrCollectionOID = sysId(12)
    val ExactlyOneOID = sysId(13)
    val OptionalOID = sysId(14)
    val QListOID = sysId(15)
    val LinkTypeOID = sysId(16)
    val TypePropOID = sysId(17)
    val CollectionPropOID = sysId(18)
    val LargeTextTypeOID = sysId(21)
    val IsModelOID = sysId(22)
    val NotInheritedOID = sysId(24)
    val AppliesToKindOID = sysId(36)
    val InternalMethodOID = sysId(38)
    val QUnitOID = sysId(39)
    val InternalPropOID = sysId(40)
    val QSetOID = sysId(79)
  }
    
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
  case class QLText(text:String) {
    def +(other:QLText) = QLText(text + other.text)
  }
  
  trait Core extends EcologyInterface {
    def ExactlyOne:Collection
    def Optional:Collection
    def QList:Collection
    def QSet:Collection
    def QUnit:Collection
    
    def UnknownType:PType[Unit]
    def InternalMethodType:PType[String] with PTypeBuilder[String,String]
    def TextType:PType[QLText] with PTypeBuilder[QLText,String]
    def LargeTextType:PType[QLText] with PTypeBuilder[QLText,String]
    def LinkType:PType[OID] with PTypeBuilder[OID,OID]
    def NameType:PType[String] with PTypeBuilder[String,String]
    def IntType:PType[Int] with PTypeBuilder[Int,Int]
    def YesNoType:PType[Boolean] with PTypeBuilder[Boolean,Boolean]
    
    def LinkFromThingBuilder:PTypeBuilderBase[OID, Thing]
    
    def QNone:QValue
    def listFrom[RT,VT](in:Iterable[RT], builder:PTypeBuilderBase[VT,RT]):QValue
    def makeListValue(cv:Iterable[ElemValue], elemT:PType[_]):QValue
    def makeSetValue(rawList:Seq[ElemValue], pt:PType[_], context:QLContext):QValue
    def followLink(context:QLContext):Option[Thing]
    // Ecots have this built-in, but non-Ecots can use this:
    def setName(str:String):(OID,QValue)
    
    def NotInheritedProp:Property[Boolean,Boolean]
    def UrProp:Property[QLText, String]
    def IsModelProp:Property[Boolean,Boolean]
    def NameProp:Property[String,String]
    def TypeProp:Property[OID,OID]
    def CollectionProp:Property[OID,OID]
    def InternalProp:Property[Boolean,Boolean]
    def AppliesToKindProp:Property[Int,Int]
    
    def emptyListOf(pType:PType[_]):QValue
    def emptyList:QValue
  }
  
  type PropList = TreeMap[Property[_,_], DisplayPropVal]
  
  /**
   * This is a collection of utilities, to build up lists of Properties for particular Things.
   */
  trait PropListManager extends EcologyInterface {
    def apply(pairs:(Property[_,_], DisplayPropVal)*):PropList
    def inheritedProps(thing:Option[Thing], model:Thing)(implicit state:SpaceState):PropList
    def from(thing:Thing)(implicit state:SpaceState):PropList
  }
}
