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
}

/**
 * @author jducoeur
 */
class AppsEcot(e:Ecology) extends QuerkiEcot(e) with SpacePluginProvider with Apps {
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
      Summary("Set to true by the system if this Thing is a shadow for a parent in an App.")))
  
  override lazy val props = Seq(
    CanUseAsAppPerm,
    CanManipulateAppsPerm,
    ShadowFlag
  )
}
