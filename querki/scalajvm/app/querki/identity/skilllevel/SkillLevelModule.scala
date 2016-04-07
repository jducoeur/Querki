package querki.identity.skilllevel.impl

import querki.identity.skilllevel._

import models.{OID, Thing, ThingState}

import querki.ecology._

import querki.spaces.CacheUpdate
import querki.util.{Contributor, Publisher, QLog}
import querki.values.{SpaceState, StateCacheKey}

class SkillLevelModule(e:Ecology) extends QuerkiEcot(e) with SkillLevel with Contributor[CacheUpdate, CacheUpdate] {
  import querki.identity.skilllevel.MOIDs._
  
  val Links = initRequires[querki.links.Links]
  val SpaceChangeManager = initRequires[querki.spaces.SpaceChangeManager]
  
  lazy val Basic = interface[querki.basic.Basic]
  lazy val DataModel = interface[querki.datamodel.DataModelAccess]
  
  override def init = {
    SpaceChangeManager.updateStateCache += this
  }
  
  override def term = {
    SpaceChangeManager.updateStateCache -= this    
  }
  
  object StateCacheKeys {
    val propsBySkill = "PropsBySkill"
  }
  
  /**
   * This gets called whenever a SpaceState is updated. We take that opportunity to build up a cached
   * mapping of Properties by SkillLevel.
   */
  def notify(evt:CacheUpdate, sender:Publisher[CacheUpdate, CacheUpdate]):CacheUpdate = {
    implicit val state = evt.current
    val calculated:Map[OID, Seq[Property[_,_]]] =
      state.propList.toSeq.
        filterNot(_.ifSet(Basic.DeprecatedProp)).
        filterNot(_.ifSet(Core.InternalProp)).
        filterNot(_.ifSet(DataModel.IsFunctionProp)).
        groupBy(apply(_))
        
    evt.updateCacheWith(MOIDs.ecotId, StateCacheKeys.propsBySkill, calculated)
  }
  
  private def publicPropsBySkill(implicit state:SpaceState):Map[OID, Seq[Property[_,_]]] = {
    state.cache.get(StateCacheKey(MOIDs.ecotId, StateCacheKeys.propsBySkill)) match {
      case Some(rawEntry) => { rawEntry.asInstanceOf[Map[OID, Seq[Property[_,_]]]] }
      case None => { QLog.error("SkillLevel couldn't find its state cache in Space " + state.id); Map.empty }
    }
  }
   
  private def propsForLevel(level:OID)(implicit state:SpaceState):Seq[Property[_,_]] = {
    publicPropsBySkill.get(level).getOrElse(Seq.empty)
  }
  def standardProps(implicit state:SpaceState):Seq[Property[_,_]] = propsForLevel(querki.identity.skilllevel.MOIDs.SkillLevelStandardOID)
  def advancedProps(implicit state:SpaceState):Seq[Property[_,_]] = propsForLevel(querki.identity.skilllevel.MOIDs.SkillLevelAdvancedOID)
  
  val SkillLevelTag = querki.identity.IdentityTag
  
  /***********************************************
   * PROPERTIES
   ***********************************************/
  
  lazy val SkillLevelProp = new SystemProperty(SkillLevelPropOID, LinkType, ExactlyOne,
    toProps(
      setName("User Level to Show This"),
      Links.LinkModelProp(skillLevelModel),
      (SkillLevelPropOID -> ExactlyOne(LinkType(SkillLevelAdvancedOID))),
      Categories(SkillLevelTag),
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
  
  lazy val skillLevelModel = ThingState(SkillLevelOID, systemOID, RootOID,
    toProps(
      setName("User Level"),
      Categories(SkillLevelTag),
      Summary("""This Model's Instances are the possible User Levels.""")))
        
  lazy val skillLevelBasic = ThingState(SkillLevelBasicOID, systemOID, SkillLevelOID,
    toProps(
      setName("User Level Basic"),
      Categories(SkillLevelTag),
      Summary("""This level should be used for the core of Querki: the powerful but easy features that everyone uses.""")))
        
  lazy val skillLevelStandard = ThingState(SkillLevelStandardOID, systemOID, SkillLevelOID,
    toProps(
      setName("User Level Standard"),
      Categories(SkillLevelTag),
      Summary("""This level should be used for the bulk of Querki: the common features that are often used, but not quite core.""")))
        
  lazy val skillLevelAdvanced = ThingState(SkillLevelAdvancedOID, systemOID, SkillLevelOID,
    toProps(
      setName("User Level Advanced"),
      Categories(SkillLevelTag),
      Summary("""This level should be used for the power-user features of Querki, which aren't quite as easy but provide the most flexibility.""")))
        
  override lazy val things = Seq(
    skillLevelModel,
    skillLevelBasic,
    skillLevelStandard,
    skillLevelAdvanced
  )
  
  /***********************************************
   * METHODS
   ***********************************************/
  
  def apply(thing:Thing)(implicit state:SpaceState):OID = {
    val result = for (
      propVal <- thing.getPropOpt(SkillLevelProp);
      levelId <- propVal.firstOpt
        )
      yield levelId
      
    result.getOrElse(SkillLevelStandard)
  } 
    
  def isAdvanced(thing:Thing)(implicit state:SpaceState):Boolean = {
    val result = for (
      propVal <- thing.getPropOpt(SkillLevelProp);
      levelId <- propVal.firstOpt;
      if (levelId == SkillLevelAdvanced)
        )
      yield true
      
    result.getOrElse(false)
  }
}