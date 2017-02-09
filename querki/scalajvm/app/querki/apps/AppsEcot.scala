package querki.apps

import akka.actor._
import akka.pattern._
import akka.util.Timeout
import scala.concurrent.duration._

import models._

import querki.api.commonName
import querki.ecology._
import querki.globals._
import querki.identity.User
import querki.spaces._
import querki.spaces.messages.SpacePluginMsg
import querki.util.{Contributor, Publisher}
import querki.values.SpaceVersion
import querki.spaces.StdSpaceCreator

object MOIDs extends EcotIds(59) {
  val CanBeAppOID = moid(1)
  val CanManipulateAppsOID = moid(2)
  val ShadowFlagOID = moid(3)
  val ShadowedThingOID = moid(4)
  
  val GalleryEntryModelOID = moid(5)
  val GallerySummaryOID = moid(6)
  val GalleryDetailsOID = moid(7)
  val GalleryOwnerOID = moid(8)
  // This is the standard OID for the Gallery Space itself:
  val GallerySpaceOID = moid(9)
  val GalleryAppIdOID = moid(10)
  
  val IsAppOID = moid(11)
  val GalleryEntryOID = moid(12)
}

/**
 * @author jducoeur
 */
class AppsEcot(e:Ecology) extends QuerkiEcot(e) with querki.core.MethodDefs with SpacePluginProvider with Apps {
  import MOIDs._
  
  val AccessControl = initRequires[querki.security.AccessControl]
  val Basic = initRequires[querki.basic.Basic]
  
  lazy val ApiRegistry = interface[querki.api.ApiRegistry]
  lazy val SpaceChangeManager = interface[querki.spaces.SpaceChangeManager]
  lazy val SpaceOps = interface[querki.spaces.SpaceOps]
  lazy val SystemManagement = interface[querki.system.SystemManagement]
  
  def PlainTextType = Basic.PlainTextType
  
  override def postInit() = {
    // Some entry points are legal without login:
    ApiRegistry.registerApiImplFor[AppsFunctions, AppsFunctionsImpl](SpaceOps.spaceRegion, false)
    SpaceChangeManager.registerPluginProvider(this)
  }
  
  override def createActors(createActorCb:CreateActorFunc):Unit = {
    // Note that we create this Singleton, and then never talk to it. It basically boots, does its
    // thing, and then doesn't do anything further.
    val (mgr, proxy) = SystemManagement.createClusterSingleton(
      createActorCb,
      StdSpaceCreator.actorProps(GallerySpaceOID, "Querki App Gallery", "Querki App Gallery", ecology),
      "GalleryCreator",
      "UnuseGalleryProxyProxy",
      PoisonPill
    )
  }
  
  /**
   * Called by each Space once, to instantiate its plugins. This is how we hook Space processing.
   */
  def createPlugin[RM[_]](space:SpaceAPI[RM], rtc:RTCAble[RM]):SpacePlugin[RM] = 
    new AppsSpacePlugin(space, rtc, ecology)
    
  /***********************************************
   * API
   ***********************************************/
  
  def addAppToSpace(user:User, spaceId:OID, appId:OID):Future[Unit] = {
    // For the time being, we simply assume that you want the current version of the App:
    SpaceOps.askSpace2(SpacePluginMsg(user, spaceId, AddApp(appId, SpaceVersion(Int.MaxValue), false, false))) {
      case AddAppResult(exOpt, _) => {
        exOpt.map(ex => throw ex)
        fut(())
      }
    }
  }
  
  def getShadowedThing(t:Thing)(implicit state:SpaceState):Thing = {
    if (t.ifSet(ShadowFlag)) {
      val model = state.anything(t.model).getOrElse(throw new Exception(s"Shadow ${t.displayName} calls for Model ${t.model}, but we can't find it!"))
      getShadowedThing(model)
    } else {
      t
    }
  }
  
  /***********************************************
   * FUNCTIONS
   ***********************************************/
  
  lazy val ShadowedThingFunction = new InternalMethod(ShadowedThingOID,
    toProps(
      setName("_shadowedThing"),
      Summary("Produces the actual Thing this Shadow is based upon."),
      SkillLevel(SkillLevelAdvanced),
      Categories(AppsTag),
      Signature(
        expected = Some(Seq(LinkType), "A List of anything"),
        reqs = Seq.empty,
        opts = Seq.empty,
        returns = (LinkType, "The Thing this Shadow is based upon, or itself if it is not a Shadow")
      ),
      Details("""See [[_isShadow._self]] for more information about Shadows.""")))
  {
    override def qlApply(inv:Invocation):QFut = {
      for {
        thing <- inv.contextAllThings
      }
        yield ExactlyOne(LinkType(getShadowedThing(thing)(inv.state)))
    }
  }
  
  /***********************************************
   * PROPERTIES
   ***********************************************/
  
  lazy val CanUseAsAppPerm = AccessControl.definePermission(CanBeAppOID, 
      commonName(_.apps.canUseAsAppPerm),
      "These people are allowed to use this Space as an App. **Use with caution! These people will be able to see everything in this Space!**",
      Seq(AccessControl.OwnerTag),
      Seq(AccessControl.AppliesToSpace),
      false,
      true)
  
  lazy val CanManipulateAppsPerm = AccessControl.definePermission(CanManipulateAppsOID, 
      commonName(_.apps.canManipulateAppsPerm),
      "These people are allowed to add or remove Apps from this Space",
      Seq(AccessControl.OwnerTag),
      Seq(AccessControl.AppliesToSpace),
      false,
      false)
      
  lazy val ShadowFlag = new SystemProperty(ShadowFlagOID, YesNoType, ExactlyOne,
    toProps(
      setName("_isShadow"),
      setInternal,
      Categories(AppsTag),
      SkillLevel(SkillLevelAdvanced),
      NotInherited,
      Summary("Set to true by the system if this Thing is a shadow for a parent in an App."),
      Details("""When a Space is based on an App, it creates local "shadows" of the Things in that App,
        |so that you can customize the Space to suit yourself. The gory details:
        |
        |* All Models in the App get local Shadows. In this case, the Shadow Model has its own OID, and
        |points to the real version in the App as *its* Model. You may enhance this Shadow Model with
        |your own custom fields.
        |
        |* Any Model Types in the App get local Shadows. The Shadow Type will have the *same* OID as
        |the real one in the App. It differs only in that it points to the local Shadow Model for that
        |Type's real Model.
        |
        |* All Properties in the App get local Shadows, but these are direct copies of the versions in the
        |App, with the same OID, so it will generally look the same to you. (The difference is mainly
        |internal -- Model Properties point to the Shadow Type.
        |
        |You should usually be able to ignore shadowing -- it is intended to usually just work -- but
        |you can use the [[_shadowedThing._self]] function to go from a Shadow to the real Thing that
        |underlies it in the App.""".stripMargin)))
  
  lazy val GallerySummary = new SystemProperty(GallerySummaryOID, PlainTextType, ExactlyOne,
    toProps(
      setName(commonName(_.apps.summaryProp)),
      setInternal,
      Categories(AppsTag),
      Summary("""This is a specialized version of the standard Summary Property, designed for describing an
        |App in the Gallery. For security reasons, it does not allow you to use any QL expressions, but you
        |can use all the usual QText markup.""".stripMargin)))
  
  lazy val GalleryDetails = new SystemProperty(GalleryDetailsOID, PlainTextType, ExactlyOne,
    toProps(
      setName(commonName(_.apps.detailsProp)),
      setInternal,
      Categories(AppsTag),
      Summary("""This is a specialized version of the standard Details Property, designed for describing an
        |App in the Gallery. For security reasons, it does not allow you to use any QL expressions, but you
        |can use all the usual QText markup.""".stripMargin)))
  
  lazy val GalleryOwner = new SystemProperty(GalleryOwnerOID, LinkType, ExactlyOne,
    toProps(
      setName("_App Gallery Owner"),
      setInternal,
      Categories(AppsTag),
      Summary("The owner of this App")))
  
  lazy val GalleryAppId = new SystemProperty(GalleryAppIdOID, LinkType, ExactlyOne,
    toProps(
      setName("_App Gallery App Id"),
      setInternal,
      Categories(AppsTag),
      Summary("The OID of this App")))
  
  lazy val GalleryEntryId = new SystemProperty(GalleryEntryOID, LinkType, ExactlyOne,
    toProps(
      setName("_Gallery Entry Id"),
      setInternal,
      Categories(AppsTag),
      Core.NotInheritedProp(true),
      Summary("This is set on the App, and contains the OID for this App in the Gallery")))
  
  lazy val IsAppFlag = new SystemProperty(IsAppOID, YesNoType, Optional,
    toProps(
      setName("_Is an App"),
      setInternal,
      Categories(AppsTag),
      Core.NotInheritedProp(true),
      Summary("Set on a Space if that Space is an App")))
  
  override lazy val props = Seq(
    ShadowedThingFunction,
      
    CanUseAsAppPerm,
    CanManipulateAppsPerm,
    ShadowFlag,
    
    GallerySummary,
    GalleryDetails,
    GalleryOwner,
    GalleryAppId,
    GalleryEntryId,
    
    IsAppFlag
  )
  
  /***********************************************
   * THINGS
   ***********************************************/
  
  lazy val GalleryEntryModel = ThingState(GalleryEntryModelOID, systemOID, Basic.SimpleThing,
    toProps(
      setName("_App Gallery Entry"),
      Core.IsModelProp(true),
      setInternal,
      Categories(AppsTag),
      Summary("An entry in the system-wide Public App Gallery"),
      Basic.DisplayNameProp("_App Gallery Entry"),
      GallerySummary(),
      GalleryDetails(),
      GalleryOwner()
    )
  )
  
  override lazy val things = Seq(
    GalleryEntryModel
  )
}
