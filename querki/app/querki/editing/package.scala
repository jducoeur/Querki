package querki

import models.{DisplayPropVal, OID, Property, PropertyBundle, Thing, Wikitext}

import querki.basic.PlainText
import querki.ecology._
import querki.values.{RequestContext, SpaceState}

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
    def getInstanceEditor(thing:PropertyBundle, rc:RequestContext, currentValue:Option[DisplayPropVal] = None):Wikitext
    
    def InstanceProps:Property[OID,OID]
    def PromptProp:Property[PlainText,String]
    
    /**
     * Given a Model, this returns the expected Properties to show/use for Instances of this Model.
     * 
     * Note that this is implicitly a "square" way of thinking of things, and doesn't account for possible
     * sub-Models that might add additional Properties, or the Properties defined on the Thing itself. So it
     * is not always the correct way to look at the problem -- use with care.
     */
    def instancePropsForModel(model:PropertyBundle, state:SpaceState):Seq[Property[_,_]]
    
    /**
     * Given a Bundle, which Properties are defined in it and *not* in its Model?
     * 
     * TODO: this really shouldn't be public, but the whole problem of "which properties are editable on this Thing?"
     * is currently scattered around too much.
     */
    def propsNotInModel(thing:PropertyBundle, state:SpaceState):Iterable[OID]
  }
}