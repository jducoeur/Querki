package querki.values

import models.{OID, Thing}
import models.system.{ExactlyOne, LinkType}

import querki.identity.skilllevel._
import querki.identity.skilllevel.MOIDs._
  
/**
 * This is a thin wrapper around the key values from SkillLevelModule, which are often useful. You usually
 * should be able to simply apply this object instead of delving into the details of SkillLevelModule.
 */
object SkillLevel {
  lazy val Basic = SkillLevelBasicOID
  lazy val Standard = SkillLevelStandardOID
  lazy val Advanced = SkillLevelAdvancedOID
  
  // This effectively duplicates SkillLevelProp.apply, but avoids runtime initialization
  // cycle problems -- it can be used from the beginning of the universe without danger.
  // This is necessary, since so many Things declare themselves to be Advanced.
  def apply(level:OID) = (SkillLevelPropOID -> ExactlyOne(LinkType(level)))
  
  def apply(thing:Thing)(implicit state:SpaceState):OID = {
    val result = for (
      propVal <- thing.getPropOpt(SkillLevelProp);
      levelId <- propVal.firstOpt
        )
      yield levelId
      
    result.getOrElse(Standard.id)
  } 
  
  def isAdvanced(thing:Thing)(implicit state:SpaceState):Boolean = {
    val result = for (
      propVal <- thing.getPropOpt(SkillLevelProp);
      levelId <- propVal.firstOpt;
      if (levelId == Advanced.id)
        )
      yield true
      
    result.getOrElse(false)
  }
}