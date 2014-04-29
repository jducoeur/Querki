package querki.uservalues

import akka.actor.Props

import models.{Kind, PType, ThingState}

import querki.ecology._
import querki.spaces.{CacheUpdate, ThingChangeRequest}
import querki.util.{Contributor, Publisher, QLog}
import querki.values.{SpaceState, StateCacheKey}

object MOIDs extends EcotIds(44) {
  val SummarizerBaseOID = moid(1)
  val UserValuePermissionOID = moid(2)
  val IsUserValueFlagOID = moid(3)
}

class UserValueEcot(e:Ecology) extends QuerkiEcot(e) with UserValues {
  import MOIDs._
  
  val AccessControl = initRequires[querki.security.AccessControl]
  val SpaceChangeManager = initRequires[querki.spaces.SpaceChangeManager]
  
  override def init = {
    SpaceChangeManager.updateStateCache += UserValueCacheUpdater
  }
  
  override def term = {
    SpaceChangeManager.updateStateCache -= UserValueCacheUpdater    
  }
  
  object StateCacheKeys {
    val userValueProps = "UserValueProps"
  }
  
  private object UserValueCacheUpdater extends Contributor[CacheUpdate, CacheUpdate] {  
    /**
     * This gets called whenever a SpaceState is updated. Figure out all the UserValue Properties (if any),
     * so that we can process them efficiently.
     */
    def notify(evt:CacheUpdate, sender:Publisher[CacheUpdate, CacheUpdate]):CacheUpdate = {
      implicit val state = evt.current
      val uvPropPairs = state.allProps.filter { pair =>
        val (pid, prop) = pair
        prop.ifSet(IsUserValueFlag)
      }
      val calculated:Set[OID] = uvPropPairs.keys.toSet
        
      evt.updateCacheWith(MOIDs.ecotId, StateCacheKeys.userValueProps, calculated)
    }
  }
  
  def isUserValueProp(propId:OID)(implicit state:SpaceState):Boolean = {
    state.cache.get(StateCacheKey(MOIDs.ecotId, StateCacheKeys.userValueProps)) match {
      case Some(rawEntry) => { rawEntry.asInstanceOf[Set[OID]].contains(propId) }
      case None => { QLog.error("UserValueEcot couldn't find its state cache in Space " + state.id); false }
    }    
  }
  
  // TODO: the following Props signature is now deprecated, and should be replaced (in Akka 2.2)
  // with "Props(classOf(Space), ...)". See:
  //   http://doc.akka.io/docs/akka/2.2.3/scala/actors.html
  def userValuePersisterProps(spaceId:OID):Props = 
    Props(new UserValuePersister(spaceId, ecology))
      
  /***********************************************
   * THINGS
   ***********************************************/
    
  /**
   * Base Model for all Summarizers, just so we can talk about them in an organized way.
   * 
   * Note that, for the moment, we aren't bothering to call this a Type, since it really isn't.
   * Is there any reason to believe this is a problem? I don't think so.
   */
  lazy val SummarizerBase = ThingState(SummarizerBaseOID, systemOID, RootOID,
    toProps(
      setName("_summarizerBase"),
      Core.IsModelProp(true),
      setInternal))
  
  override lazy val things = Seq(
    SummarizerBase
  )
    
  /***********************************************
   * PROPERTIES
   ***********************************************/
  
  lazy val UserValuePermission = AccessControl.definePermission(
      UserValuePermissionOID, 
      "Who Can Have User Values", 
      "Who is allowed to define their own User Values (such as Ratings or Reviews)",
      Seq(AccessControl.OwnerTag, AccessControl.MembersTag), false)
      
  lazy val IsUserValueFlag = new SystemProperty(IsUserValueFlagOID, YesNoType, ExactlyOne,
      toProps(
        setName("Is User Value Property"),
        AppliesToKindProp(Kind.Property),
        SkillLevel(SkillLevelAdvanced),
        Summary("Add this flag to a Property, and set it to true, if this Property should have a separate value for each user.")))

  override lazy val props = Seq(
    UserValuePermission,
    IsUserValueFlag
  )
}