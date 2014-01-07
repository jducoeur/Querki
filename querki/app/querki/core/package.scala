package querki

import collection.immutable.TreeMap

import models.{DisplayPropVal, Property, Thing}

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
    val UrPropOID = sysId(5)
    val NotInheritedOID = sysId(24)
    val ApplyMethodOID = sysId(46)
  }
  
  trait Core extends EcologyInterface {
    def ApplyMethod:Property[QLText,String]
    def NotInheritedProp:Property[Boolean,Boolean]
    def UrProp:Property[QLText, String]
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
