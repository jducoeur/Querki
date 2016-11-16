package querki.apps

import akka.pattern._
import akka.util.Timeout
import scala.concurrent.duration._

import models.{Property}

import querki.api.commonName
import querki.ecology._
import querki.globals._
import querki.identity.User
import querki.spaces._
import querki.spaces.messages.SpacePluginMsg
import querki.util.{Contributor, Publisher}
import querki.values.SpaceVersion

object MOIDs extends EcotIds(59) {
  val CanBeAppOID = moid(1)
  val CanManipulateAppsOID = moid(2)
  val ShadowFlagOID = moid(3)
  val ShadowedThingOID = moid(4)
}

/**
 * @author jducoeur
 */
class AppsEcot(e:Ecology) extends QuerkiEcot(e) with querki.core.MethodDefs with SpacePluginProvider with Apps {
  import MOIDs._
  
  val AccessControl = initRequires[querki.security.AccessControl]
  
  lazy val ApiRegistry = interface[querki.api.ApiRegistry]
  lazy val SpaceChangeManager = interface[querki.spaces.SpaceChangeManager]
  lazy val SpaceOps = interface[querki.spaces.SpaceOps]
  
  override def postInit() = {
    // Some entry points are legal without login:
    ApiRegistry.registerApiImplFor[AppsFunctions, AppsFunctionsImpl](SpaceOps.spaceRegion, false)
    SpaceChangeManager.registerPluginProvider(this)
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
    SpaceOps.askSpace2(SpacePluginMsg(user, spaceId, AddApp(appId, SpaceVersion(Int.MaxValue)))) {
      case AddAppResult(exOpt) => {
        exOpt.map(ex => throw ex)
        fut(())
      }
    }
  }
  
  def getShadowedThing(t:Thing)(implicit state:SpaceState):Thing = {
    if (t.ifSet(ShadowFlag)) {
      val model = state.anything(t.model).getOrElse(throw new Exception(s"Shadow ${t.displayName} doesn't have a Model we can find!"))
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
  
  override lazy val props = Seq(
    ShadowedThingFunction,
      
    CanUseAsAppPerm,
    CanManipulateAppsPerm,
    ShadowFlag
  )
}
