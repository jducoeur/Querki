package querki.values

import models.OID

import modules.Modules.SkillLevel._
  
/**
 * This is a thin wrapper around the key values from SkillLevelModule, which are often useful. You usually
 * should be able to simply apply this object instead of delving into the details of SkillLevelModule.
 */
object SkillLevel {
  lazy val Basic = skillLevelBasic
  lazy val Standard = skillLevelStandard
  lazy val Advanced = skillLevelAdvanced
  
  def apply(level:OID) = skillLevelProp(level)
}