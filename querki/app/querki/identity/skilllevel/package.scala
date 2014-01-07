package querki.identity

import models.{OID, Property, Thing}

import querki.ecology._

import querki.values.SpaceState

package object skilllevel {
  object MOIDs extends EcotIds(14) {
    val SkillLevelPropOID = moid(1)
    val SkillLevelOID = moid(2)
    val SkillLevelBasicOID = moid(3)
    val SkillLevelStandardOID = moid(4)
    val SkillLevelAdvancedOID = moid(5)
  }
  
  trait SkillLevel extends EcologyInterface {  
    def SkillLevelProp:Property[OID, OID]
    
    def apply(thing:Thing)(implicit state:SpaceState):OID
    def isAdvanced(thing:Thing)(implicit state:SpaceState):Boolean
  }
}