package querki

import models.{OID, Property}

import querki.ecology._

import querki.basic.PlainText

package object editing {

  object MOIDs extends EcotIds(13) {
    // Previously in System
    val PlaceholderTextOID = sysId(19)
    val PromptOID = sysId(20)
    val EditMethodOID = sysId(42)
    val InstanceEditPropsOID = sysId(47)
    val FormLineMethodOID = sysId(81) 
    val EditOrElseMethodOID = sysId(82)
    
    val EditAsPickListOID = moid(1)
    val InstanceEditViewOID = moid(2)
    val EditWidthPropOID = moid(3)
  }

  trait Editor extends EcologyInterface {
    def InstanceEditPropsProp:Property[OID,OID]
    def PromptProp:Property[PlainText,String]
  }
}