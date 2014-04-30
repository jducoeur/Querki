package querki.uservalues

import akka.actor.Actor.Receive
import akka.actor.Props
import akka.event.LoggingReceive

import models.{Kind, PType, ThingState}
import models.Thing.PropMap

import querki.ecology._
import querki.identity.SystemUser
import querki.spaces.{CacheUpdate, SpaceAPI, SpacePlugin, SpacePluginProvider, ThingChangeRequest}
import querki.spaces.messages.SpacePluginMsg
import querki.util.{Contributor, Publisher, QLog}
import querki.values.{SpaceState, StateCacheKey}

object MOIDs extends EcotIds(44) {
  val SummarizerBaseOID = moid(1)
  val UserValuePermissionOID = moid(2)
  val IsUserValueFlagOID = moid(3)
  val SummaryLinkOID = moid(4)
  val SummarizesPropertyLinkOID = moid(5)
}

class UserValueEcot(e:Ecology) extends QuerkiEcot(e) with UserValues with SpacePluginProvider {
  import MOIDs._
  
  val AccessControl = initRequires[querki.security.AccessControl]
  val Links = initRequires[querki.links.Links]
  val SpaceChangeManager = initRequires[querki.spaces.SpaceChangeManager]
  
  override def init = {
    SpaceChangeManager.updateStateCache += UserValueCacheUpdater
    SpaceChangeManager.registerPluginProvider(this)
  }
  
  override def term = {
    SpaceChangeManager.updateStateCache -= UserValueCacheUpdater    
  }
  
  def createPlugin(space:SpaceAPI):SpacePlugin = {
    new UserValueSpacePlugin(space)
  }
  
  /**
   * When a UserSession changes a UserValue, it may send a SummarizeChange message to the Space, telling it to
   * update the Summary for that Property. This handles the message in a plugin, so the Space code doesn't
   * need to know about all that.
   */
  class UserValueSpacePlugin(s:SpaceAPI) extends SpacePlugin(s) {
    def asSummarizer[UVT,VT](prop:Property[VT,_], msg:SummarizeChange[UVT]):Option[(Property[VT,_], Summarizer[UVT,VT])] = {
      if (prop.pType.isInstanceOf[Summarizer[UVT,VT]])
        Some((prop.asInstanceOf[Property[VT,_]], prop.pType.asInstanceOf[Summarizer[UVT,VT]]))
      else
        None
    }
    
    def receive = {
      case SpacePluginMsg(msg @ SummarizeChange(tid, fromProp, summaryId, previous, current)) => {
        implicit val state = space.state
        for {
          rawProp <- state.prop(summaryId) orElse QLog.warn(s"UserValueSpacePlugin didn't find requested Summary Property $summaryId")
          thing <- state.anything(tid) orElse QLog.warn(s"UserValueSpacePlugin didn't find requested Thing $tid")
          (summaryProp, summarizer) <- asSummarizer(rawProp, msg)
          newSummary = summarizer.addToSummary(tid, fromProp, summaryProp, previous, current)
          newProps = thing.props + (summaryProp.id -> newSummary)
        }
          space.modifyThing(SystemUser, tid, None, (t:Thing) => newProps)
      }
    }
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
  
  lazy val SummaryLink = new SystemProperty(SummaryLinkOID, LinkType, ExactlyOne,
      toProps(
        setName("Summary Link"),
        AppliesToKindProp(Kind.Property),
        Links.LinkKindProp(Kind.Property),
        SkillLevel(SkillLevelAdvanced),
        Summary("Link to the Summary of this User Value"),
        Details("""A User Value Property contains a separate value for each User. For instance, the Rating Property
            |allows each User to give their own Rating to a given Thing. That is useful, but you often want to be
            |able to look at the statistics about those Ratings. That is where the Summary comes in.
            |
            |The Summary stores the aggregate information about the User Value that points to it, and provides you
            |with functions such as _average to use with that information.
            |
            |This is a very advanced Property, and you should only use it if you know what you are doing. The Summary's
            |Type must be compatible with that of the User Value it is summarizing. In the long run, we will wrap all
            |of this in an easier-to-use UI.""".stripMargin)))
  
  lazy val SummarizesPropertyLink = new SystemProperty(SummarizesPropertyLinkOID, LinkType, ExactlyOne,
      toProps(
        setName("Summarizes Property"),
        AppliesToKindProp(Kind.Property),
        Links.LinkKindProp(Kind.Property),
        SkillLevel(SkillLevelAdvanced),
        Summary("Optional pointer from a Summary Property to the Property that it summarizes"),
        Details("""This link is sometimes necessary when you have a User Value Property that is a Model Type.
            |If you want to summarize such a Property (in order to examine its statistics), you aren't actually
            |summarizing the entire Property -- you're just summarizing a Property *inside* that Model Type.
            |This lets you specify *which* Property of the Model Type should be summarized.
            |
            |For example, take the Review Property. The summary for that isn't based on the entire Review -- it
            |is based on the Rating Property *inside* the Review. So the Summary needs to point to Rating, to
            |know what to do.
            |
            |This is a very advanced Property, intended only for people who are building complex User Value Properties.""".stripMargin)))

  override lazy val props = Seq(
    UserValuePermission,
    IsUserValueFlag,
    SummaryLink,
    SummarizesPropertyLink
  )
}