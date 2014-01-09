package querki

import collection.immutable.TreeMap

import models.{DisplayPropVal, OID, Property, Thing}

import models.system.{QLText}
import models.system.OIDs.sysId

import querki.ecology._

import querki.values.SpaceState

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
    val TypePropOID = sysId(17)
    val CollectionPropOID = sysId(18)
    val IsModelOID = sysId(22)
    val NotInheritedOID = sysId(24)
    val AppliesToKindOID = sysId(36)
    val InternalPropOID = sysId(40)
    val ApplyMethodOID = sysId(46)
  }
  
  trait Core extends EcologyInterface {
    def ApplyMethod:Property[QLText,String]
    def NotInheritedProp:Property[Boolean,Boolean]
    def UrProp:Property[QLText, String]
    def IsModelProp:Property[Boolean,Boolean]
    def NameProp:Property[String,String]
    def TypeProp:Property[OID,OID]
    def CollectionProp:Property[OID,OID]
    def InternalProp:Property[Boolean,Boolean]
    def AppliesToKindProp:Property[Int,Int]
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
