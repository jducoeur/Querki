package querki.values

import models.{OID, Thing}

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
  def apply(thing:Thing)(implicit state:SpaceState):OID = {
    val result = for (
      propVal <- thing.getPropOpt(skillLevelProp);
      levelId <- propVal.firstOpt
        )
      yield levelId
      
    result.getOrElse(Standard.id)
  } 
  
  def isAdvanced(thing:Thing)(implicit state:SpaceState):Boolean = {
    val result = for (
      propVal <- thing.getPropOpt(skillLevelProp);
      levelId <- propVal.firstOpt;
      if (levelId == Advanced.id)
        )
      yield true
      
    result.getOrElse(false)
  }
}