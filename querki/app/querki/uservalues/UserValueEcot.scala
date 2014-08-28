package querki.uservalues

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import akka.actor.Actor.Receive
import akka.actor.Props
import akka.event.LoggingReceive

import models.{Kind, PropertyBundle, PType, ThingState, Wikitext}
import models.Thing.PropMap

import querki.ecology._
import querki.identity.SystemUser
import querki.ql.InvocationValue
import querki.spaces.{CacheUpdate, SpaceAPI, SpacePlugin, SpacePluginProvider, ThingChangeRequest}
import querki.spaces.messages.{SpacePluginMsg, UserValuePersistRequest}
import querki.types.{ModeledPropertyBundle, SimplePropertyBundle}
import querki.uservalues.PersistMessages._
import querki.util.{Contributor, Publisher, QLog}
import querki.util.ActorHelpers._
import querki.values.{QLContext, SpaceState, StateCacheKey}

object MOIDs extends EcotIds(44) {
  val SummarizerBaseOID = moid(1)
  val UserValuePermissionOID = moid(2)
  val IsUserValueFlagOID = moid(3)
  val SummaryLinkOID = moid(4)
  val SummarizesPropertyLinkOID = moid(5)
  
  val UserValuesFunctionOID = moid(6)
  val ThingValuesFunctionOID = moid(7)
  val UserValueFunctionOID = moid(8)
  val UserValueModelOID = moid(9)
  val UserValueTypeOID = moid(10)
  val UVUserPropOID = moid(11)
  val UVThingPropOID = moid(12)
  val UVValPropOID = moid(13)
  val UVPropPropOID = moid(14)
  val UpdatePropSummariesFunctionOID = moid(15)
}

class UserValueEcot(e:Ecology) extends QuerkiEcot(e) with UserValues with SpacePluginProvider with querki.core.MethodDefs with querki.types.ModelTypeDefiner {
  import MOIDs._
  
  val AccessControl = initRequires[querki.security.AccessControl]
  val Basic = initRequires[querki.basic.Basic]
  val IdentityAccess = initRequires[querki.identity.IdentityAccess]
  val Links = initRequires[querki.links.Links]
  val SpaceChangeManager = initRequires[querki.spaces.SpaceChangeManager]
  val Types = initRequires[querki.types.Types]
  
  lazy val Html = interface[querki.html.HtmlUI]
  lazy val SpaceOps = interface[querki.spaces.SpaceOps]
  
  lazy val IdentityType = IdentityAccess.IdentityType
  
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
  
  case class RecalculateSummaries[UVT](fromProp:Property[UVT,_], summaryId:OID, values:Seq[OneUserValue])
  
  /**
   * When a UserSpaceSession changes a UserValue, it may send a SummarizeChange message to the Space, telling it to
   * update the Summary for that Property. This handles the message in a plugin, so the Space code doesn't
   * need to know about all that.
   */
  class UserValueSpacePlugin(s:SpaceAPI) extends SpacePlugin(s) {
    // This is a bit weird and hairy, but allows us to use the received message's type parameters to sanity-check that
    // the Summarizer Property actually matches the values we are passing in to it:
    def asSummarizer[UVT,MSG[UVT],VT](prop:Property[VT,_], msg:MSG[UVT]):Option[(Property[VT,_], Summarizer[UVT,VT])] = {
      if (prop.pType.isInstanceOf[Summarizer[UVT,VT]])
        Some((prop.asInstanceOf[Property[VT,_]], prop.pType.asInstanceOf[Summarizer[UVT,VT]]))
      else
        None
    }
    
    def receive = {
      case SpacePluginMsg(_, _, _, msg @ SummarizeChange(tid, fromProp, summaryId, previous, current)) => {
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
      
      case SpacePluginMsg(_, _, _, msg @ RecalculateSummaries(fromProp, summaryId, values)) => {
        implicit val state = space.state
        for {
          rawProp <- state.prop(summaryId) orElse QLog.warn(s"UserValueSpacePlugin didn't find requested Summary Property $summaryId")
          (summaryProp, summarizer) <- asSummarizer(rawProp, msg)
          // IMPORTANT: note that we'll get one result for each change that is needed. This can produce a
          // non-trivial number of modifyThing requests:
          (tid, newSummary) <- summarizer.recalculate(fromProp, summaryProp, values)
        }
          space.modifyThing(SystemUser, tid, None, ((t:Thing) => t.props + (summaryProp.id -> newSummary)))
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
      
  lazy val UserValueModel = ThingState(UserValueModelOID, systemOID, RootOID,
    toProps(
      setName("_userValueModel"),
      Core.IsModelProp(true),
      setInternal,
      UVUserProp(),
      UVThingProp(),
      UVPropProp(),
      UVValProp(),
      Summary("The _userValues and _thingValues functions return Sets of _userValueModel."),
      Details("""This contains these Properties:
          |* _userValueUser -- who set this value
          |* _userValueThing -- the Thing that this value is on
          |* _userValueProperty -- the Property that was set
          |* _userValueValue -- the actual value""".stripMargin)))
  
  override lazy val things = Seq(
    SummarizerBase,
    UserValueModel
  )
  
  /***********************************************
   * TYPES
   ***********************************************/
  
  lazy val UserValueType = new ModelType(UserValueTypeOID, UserValueModelOID,
    toProps(
      setName("_userValueType"),
      setInternal,
      Summary("The Type you get from the _userValues and _thingValues functions.")))
  {
    override def doWikify(context:QLContext)(bundle:ModeledPropertyBundle, displayOpt:Option[Wikitext], lexicalThing:Option[PropertyBundle] = None) = {
      implicit val state = context.state
      val result = for {
        identityPV <- bundle.getPropOpt(UVUserProp)
        identityRendered = identityPV.v.wikify(context, displayOpt, lexicalThing)
        thingPV <- bundle.getPropOpt(UVThingProp)
        thingRendered = thingPV.v.wikify(context, displayOpt, lexicalThing)
        propPV <- bundle.getPropOpt(UVPropProp)
        propRendered = propPV.v.wikify(context, displayOpt, lexicalThing)
        valPV <- bundle.getPropOpt(UVValProp)
        valRendered = valPV.v.wikify(context, displayOpt, lexicalThing)
      }
        yield identityRendered + Wikitext(" -- ") + thingRendered + Wikitext(":") + propRendered +
          Wikitext.nl + Wikitext.nl + valRendered + Wikitext.nl + Wikitext.nl
          
      result.getOrElse(Wikitext.empty)
    }
  }
  
  override lazy val types = Seq(
    UserValueType
  )
  
  /***********************************************
   * FUNCTIONS
   ***********************************************/
  
  /**
   * TODO: EEEEVIL! This function, and the one below it, involve blocking calls! This is horrible,
   * but kind of necessary at the moment. The implication is that the QL pipeline needs to become
   * asynchronous. That will be a *major* pain in the ass, but it's going to have to happen. The notion
   * should probably be that each Stage results in a Future, which is *usually* immediately redeemed,
   * but can be put off.
   * 
   * Moreover, this really drives home that the QL processing should happen inside of a worker Actor
   * affiliated with the Space under normal circumstances. As it is, we have to drill a hole through
   * to the stateful Actor side of the world, which is pretty serious stuff.
   */
  lazy val UserValuesFunction = new InternalMethod(UserValuesFunctionOID,
    toProps(
      setName("_userValues"),
      Summary("Fetch all of the User Values for this Property on this Thing, for all Users"),
      Details("""    THING -> PROP._userValues -> USER VALUES""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QValue = {
      for {
        // First, figure out the Thing and Prop we're working with:
        thingId <- inv.contextAllAs(LinkType)
        prop <- inv.definingContextAsProperty
        msg = UserValuePersistRequest(
                inv.context.request.requesterOrAnon, inv.state.ownerIdentity.map(_.id).get, inv.state.toThingId, 
                LoadThingPropValues(thingId, prop.id, inv.state))
        // Here is the moment of great evil -- this is actually a blocking request to the Space's User Value Persister:
        uv <-  inv.iter(SpaceOps.spaceManager.askBlocking(msg) {
                 case ValuesForUser(uvs) => uvs
               })
        // Finally, transform the results into a QL-pipeline-friendly form:
        uvInstance = UserValueType(SimplePropertyBundle(
                       UVUserProp(uv.identity),
                       UVThingProp(uv.thingId),
                       UVPropProp(uv.propId),
                       UVValProp(uv.v)))
      }
        yield ExactlyOne(uvInstance)
    }
  }
  
  lazy val ThingValuesFunction = new InternalMethod(ThingValuesFunctionOID,
    toProps(
      setName("_thingValues"),
      Summary("Fetch all of this User's User Values"),
      Details("""    IDENTITY -> _thingValues -> USER VALUES
          |
          |Note that, for the moment, this fetches all of the User Values for *all* Properties. We will
          |probably add a Property-specific variant eventually, but this seems more generally useful.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QValue = {
      for {
        identity <- inv.contextAllAs(IdentityType)
        msg = UserValuePersistRequest(
                inv.context.request.requesterOrAnon, inv.state.ownerIdentity.map(_.id).get, inv.state.toThingId, 
                LoadUserPropValues(identity, inv.state))
        // Here is the moment of great evil -- this is actually a blocking request to the Space's User Value Persister:
        uv <-  inv.iter(SpaceOps.spaceManager.askBlocking(msg) {
                 case ValuesForUser(uvs) => uvs
               })
        // Finally, transform the results into a QL-pipeline-friendly form:
        uvInstance = UserValueType(SimplePropertyBundle(
                       UVUserProp(uv.identity),
                       UVThingProp(uv.thingId),
                       UVPropProp(uv.propId),
                       UVValProp(uv.v)))
      }
        yield ExactlyOne(uvInstance)
    }
  }
  
  lazy val UpdatePropSummariesFunction = new InternalMethod(UpdatePropSummariesFunctionOID,
    toProps(
      setName("_updatePropSumaries"),
      setInternal,
      Summary("Check the Summaries for all of the User Values for this Property, for all Things."),
      Details("""This function is expensive, and should not be called unless you have reason to believe that
          |something has gotten out of sync. It is usually invoked from a button on the Property's own page.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QValue = {
      // Make sure that the parameters are valid...
      val prepInv:InvocationValue[(Property[_,_], OID)] = for {
        prop <- inv.preferDefiningContext.definingContextAsProperty
        // Make sure the specified Property *has* a summary:
        summaryPropPV <- inv.opt(prop.getPropOpt(SummaryLink)(inv.state))
        summaryPropId <- inv.opt(summaryPropPV.firstOpt)        
      }
        yield (prop, summaryPropId)
      
      prepInv.get.headOption match {
        case Some((prop, summaryId)) => {
          // ... go fetch the actual User Values for this Property...
          val msg = UserValuePersistRequest(
                  inv.context.request.requesterOrAnon, inv.state.ownerIdentity.map(_.id).get, inv.state.toThingId, 
                  LoadAllPropValues(prop, inv.state))
          // IMPORTANT: note that this wanders off into asynchrony. It is *not* fundamentally evil, since we don't
          // block on the response, but we don't actually give the user any interesting feedback.
          // TODO: once QL has the ability to cope with asynchronous functions, this should become one.
          val fut = SpaceOps.askSpaceManager2(msg) {
            case ValuesForUser(values) => {
              // ... and tell the SpaceManager to recompute the Summaries. (Note that the handler for this is above.)
	          val msg = SpacePluginMsg(inv.context.request.requesterOrAnon, inv.state.owner, inv.state.id, RecalculateSummaries(prop, summaryId, values))
	          // End of the line -- just fire and forget at this point:
	          SpaceOps.spaceManager ! msg
	          Future.successful {}
            }
            case other => QLog.error(s"LoadAllPropValues for ${prop.displayName} in Space ${inv.state} got response $other"); Future.successful {}
          }
          // Force the Future to evaluate once it is ready:
          fut.onComplete(t => {})
          
          Html.HtmlValue(s"""<div class="alert">
            |<button type="button" class="close" data-dismiss="alert">&times;</button>
            |Rebuilding User Value Summaries for ${prop.displayName}. This should be ready in a moment.
            |</div>""".stripMargin) 
        }
        case None => {
          // Huh -- how did we get here? The button to invoke _updatePropSummaries is displayed in CoreEcot, and isn't
          // supposed to display unless this Property has a Summary Link:
          Html.HtmlValue(s"""<div class="alert">
            |<button type="button" class="close" data-dismiss="alert">&times;</button>
            |ERROR: That Property doesn't have a Summary Link defined!
            |</div>""".stripMargin)          
        }
      }
    }
  }
    
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
  
  lazy val UVUserProp = new SystemProperty(UVUserPropOID, IdentityAccess.IdentityType, ExactlyOne,
      toProps(
        setName("_userValueUser"),
        setInternal,
        Summary("The User who set a particular User Value")))
  
  lazy val UVThingProp = new SystemProperty(UVThingPropOID, LinkType, ExactlyOne,
      toProps(
        setName("_userValueThing"),
        setInternal,
        Summary("The Thing that a User Value is set on")))
  
  lazy val UVValProp = new SystemProperty(UVValPropOID, Types.WrappedValueType, ExactlyOne,
      toProps(
        setName("_userValueValue"),
        setInternal,
        Summary("The actual value of a User Value")))
  
  lazy val UVPropProp = new SystemProperty(UVPropPropOID, LinkType, ExactlyOne,
      toProps(
        setName("_userValueProperty"),
        setInternal,
        Summary("The Property of a User Value")))

  override lazy val props = Seq(
    UserValuesFunction,
    ThingValuesFunction,
    UpdatePropSummariesFunction,
      
    UserValuePermission,
    IsUserValueFlag,
    SummaryLink,
    SummarizesPropertyLink,
    
    UVUserProp,
    UVThingProp,
    UVValProp,
    UVPropProp
  )
}