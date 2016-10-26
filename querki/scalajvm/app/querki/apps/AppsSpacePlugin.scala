package querki.apps

import akka.actor._

import models.ModelPersistence
import models.Thing._

import querki.globals._
import querki.identity.{IdentityPersistence, User}
import querki.spaces._
import SpaceMessagePersistence.DHAddApp
import querki.spaces.messages._
import querki.time._
import querki.util.PublicException
import querki.values.{RequestContext, SpaceVersion}

/**
 * This code runs *inside* the Space Actor, as a plugin. It has important constraints, as described in
 * SpaceAPI.
 * 
 * @author jducoeur
 */
class AppsSpacePlugin[RM[_]](api:SpaceAPI[RM], rtc:RTCAble[RM], implicit val ecology:Ecology) 
  extends SpacePlugin(api, rtc) with ModelPersistence with IdentityPersistence with querki.types.ModelTypeDefiner with EcologyMember with AppsPure
{
  lazy val AccessControl = interface[querki.security.AccessControl]
  lazy val Apps = interface[Apps]
  lazy val SpaceOps = interface[querki.spaces.SpaceOps]
  
  implicit def rm2rtc[A](rm:RM[A]) = rtc.toRTC(rm)
  
  def addApp(req:User, appId:OID, appVersion:SpaceVersion)(state:SpaceState):RM[ChangeResult] = {
    // Belt-and-suspenders security check that the current user is allowed to do this. In theory, this
    // can only fail if something has changed significantly:
    if (!AccessControl.hasPermission(Apps.CanManipulateAppsPerm, space.state, req, space.state))
      throw new PublicException("Apps.notAllowed")
    
    // Okay -- load the app:
    api.loadAppVersion(appId, appVersion, state.allApps).map { app =>
      // Secondary check: is this App willing to be used?
      if (AccessControl.hasPermission(Apps.CanUseAsAppPerm, app, space.state.ownerIdentity.get.id, app)) {
        implicit val s = state
        val dhApp = dh(app)
        val dhParents = app.allApps.values.toSeq.map(dh(_))
        val msg = DHAddApp(req, DateTime.now, dhApp, dhParents)
        ChangeResult(List(msg), Some(appId), addFilledAppPure(app)(s))
      } else {
        // This App doesn't acknowledge that this person is allowed to use it:
        throw new PublicException("Apps.notAnApp")
      }
    }    
  }
  
  /**
   * This will be called during the Actor's receive loop, and provides supplementary
   * handlers particular to Apps.
   */
  def receive:Actor.Receive = {
    case SpacePluginMsg(req, _, AddApp(appId, appVersion)) => {
      api.runAndSendResponse("addApp", addApp(req, appId, appVersion))(api.state)
    }
  }
}

/**
 * Sent from AppsFunctions to the Plugin, so that app-adding can happen inside the Space's own context.
 * To add the current version of the App, use Int.MaxValue as the version.
 */
private [apps] case class AddApp(appId:OID, appVersion:SpaceVersion)
