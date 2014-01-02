package querki.identity

import models.{OID, PropertyInterface}

import modules.ModuleIds

package object skilllevel {
  object MOIDs extends ModuleIds(14) {
    val SkillLevelPropOID = moid(1)
    val SkillLevelOID = moid(2)
    val SkillLevelBasicOID = moid(3)
    val SkillLevelStandardOID = moid(4)
    val SkillLevelAdvancedOID = moid(5)
  }
  
  val SkillLevelProp = new PropertyInterface[OID, OID]
}