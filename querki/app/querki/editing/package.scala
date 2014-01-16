package querki

import models.{OID, Property, Thing, Wikitext}

import querki.basic.PlainText
import querki.ecology._
import querki.values.RequestContext

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
    /**
     * This fetches the wikitext UI for editing the given Thing. Assumes that the caller has already validated
     * that the request comes from someone who can edit this Thing.
     */
    def getInstanceEditor(thing:Thing, rc:RequestContext):Wikitext
    
    def InstanceEditPropsProp:Property[OID,OID]
    def PromptProp:Property[PlainText,String]
  }
}