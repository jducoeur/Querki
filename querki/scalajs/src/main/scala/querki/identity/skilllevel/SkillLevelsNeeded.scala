package querki.identity.skilllevel

import querki.globals._

/**
 * A little mix-in trait providing access to the Skill Level system.
 */
trait SkillLevelsNeeded extends EcologyMember {
  lazy val SkillLevel = interface[querki.identity.skilllevel.SkillLevel]

  def userSkillLevel = SkillLevel.current
  lazy val Easy = SkillLevel.EasyComplexity
  lazy val Standard = SkillLevel.StandardComplexity
  lazy val Advanced = SkillLevel.AdvancedComplexity
}
