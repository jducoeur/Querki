package querki

import collection.immutable.TreeMap

import models.{Collection, DisplayPropVal, OID, Property, PType, PTypeBuilderBase, Thing}

import models.system.{QLText}
import models.system.OIDs.sysId

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
    val UrPropOID = sysId(5)
    val NameOID = sysId(6)
    val UrCollectionOID = sysId(12)
    val ExactlyOneOID = sysId(13)
    val OptionalOID = sysId(14)
    val QListOID = sysId(15)
    val TypePropOID = sysId(17)
    val CollectionPropOID = sysId(18)
    val IsModelOID = sysId(22)
    val NotInheritedOID = sysId(24)
    val AppliesToKindOID = sysId(36)
    val QUnitOID = sysId(39)
    val InternalPropOID = sysId(40)
    val ApplyMethodOID = sysId(46)
    val QSetOID = sysId(79)
  }
  
  trait Core extends EcologyInterface {
    def ExactlyOne:Collection
    def Optional:Collection
    def QList:Collection
    def QSet:Collection
    def QUnit:Collection
    
    def QNone:QValue
    def listFrom[RT,VT](in:Iterable[RT], builder:PTypeBuilderBase[VT,RT]):QValue
    def makeListValue(cv:Iterable[ElemValue], elemT:PType[_]):QValue
    def makeSetValue(rawList:Seq[ElemValue], pt:PType[_], context:QLContext):QValue
    
    def ApplyMethod:Property[QLText,String]
    
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
