package querki

import models.{OID, Property}

import querki.ecology._

package object links {
  object MOIDs extends EcotIds(27) {
    val LinkKindOID = sysId(33)
    val LinkAllowAppsOID = sysId(34)
    val LinkModelOID = sysId(35)
    val LinkToModelsOnlyOID = sysId(70)
    val NoCreateThroughLinkOID = sysId(103)
  }
  
  trait Links extends EcologyInterface {
    def LinkAllowAppsProp:Property[Boolean,Boolean]
    def LinkKindProp:Property[Int,Int]
    def LinkModelProp:Property[OID,OID]
    def LinkToModelsOnlyProp:Property[Boolean,Boolean]
    def NoCreateThroughLinkProp:Property[Boolean,Boolean]
  }
}