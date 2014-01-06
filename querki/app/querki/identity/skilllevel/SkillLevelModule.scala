package querki.identity.skilllevel.impl

import querki.identity.skilllevel._

import models.{ThingState}
import models.Thing._
import models.system.ExactlyOne
import models.system.LinkType
import models.system.{DisplayTextProp, LinkModelProp, SystemProperty}
import models.system.OIDs.{systemOID}

import querki.ecology._

class SkillLevelModule(e:Ecology) extends QuerkiEcot(e) with SkillLevel {
  import querki.identity.skilllevel.MOIDs._
  
  /***********************************************
   * PROPERTIES
   ***********************************************/
  
  lazy val SkillLevelProp = new SystemProperty(SkillLevelPropOID, LinkType, ExactlyOne,
      toProps(
        setName("User Level to Show This"),
        LinkModelProp(skillLevelModel),
        (SkillLevelPropOID -> ExactlyOne(LinkType(SkillLevelAdvancedOID))),
        Summary("Describes the complexity of this Thing, in terms of what level of user experience it wants"),
        Details("""Querki tries very hard to be simple, but at the same time includes a great deal of power.
            |The way it tames some of that complexity is with the notion of "user level" -- basically, providing a
            |way for the user to say how much complexity they want to tangle with.
            |
            |There are three standard levels:
            |* **Basic** -- the most core elements of Querki, stripped down to be easy to use.
            |* **Standard** -- the bulk of the system, but without the persnickety details that most people don't
            |    want to know about.
            |* **Advanced** -- the deepest recesses of Querki, which give a bunch of extra flexibility and power.
            |    Mainly intended for programmers.
            |
            |When applied to a Property, User Level to Show This says how advanced that Property is; it will be hidden away if
            |the user's level is set below that.
            |
            |As of this writing, the mechanism to set your Level is not there yet, but it will come in due course.""".stripMargin)))

  override lazy val props = Seq(
    SkillLevelProp
  )
  
  /***********************************************
   * THINGS
   ***********************************************/
  
  lazy val skillLevelModel = ThingState(SkillLevelOID, systemOID, querki.basic.MOIDs.SimpleThingOID,
      toProps(
        setName("User Level"),
        DisplayTextProp("""This Model's Instances are the possible User Levels.""")))
        
  lazy val skillLevelBasic = ThingState(SkillLevelBasicOID, systemOID, SkillLevelOID,
      toProps(
        setName("User Level Basic"),
        DisplayTextProp("""This level should be used for the core of Querki: the powerful but easy features that everyone uses.""")))
        
  lazy val skillLevelStandard = ThingState(SkillLevelStandardOID, systemOID, SkillLevelOID,
      toProps(
        setName("User Level Standard"),
        DisplayTextProp("""This level should be used for the bulk of Querki: the common features that are often used, but not quite core.""")))
        
  lazy val skillLevelAdvanced = ThingState(SkillLevelAdvancedOID, systemOID, SkillLevelOID,
      toProps(
        setName("User Level Advanced"),
        DisplayTextProp("""This level should be used the power-user features of Querki, which aren't quite as easy but provide the most flexibility.""")))
        
  override lazy val things = Seq(
    skillLevelModel,
    skillLevelBasic,
    skillLevelStandard,
    skillLevelAdvanced
  )
}