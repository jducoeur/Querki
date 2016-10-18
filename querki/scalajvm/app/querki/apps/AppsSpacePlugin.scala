package querki.apps

import akka.actor._

import models.ModelPersistence
import models.Thing._

import querki.globals._
import querki.identity.IdentityPersistence
import querki.spaces._
import SpaceMessagePersistence.DHAddApp
import querki.spaces.messages._
import querki.time._
import querki.util.PublicException
import querki.values.{RequestContext, SpaceVersion}

/**
 * This code runs *inside* the Space Actor, as a plugin. It has important constraints, as described in
 * SpaceAPI. It plays a bit fast and loose with those constraints, since it is doing such dramatic
 * violence to the Space.
 * 
 * @author jducoeur
 */
class AppsSpacePlugin[RM[_]](api:SpaceAPI[RM], rtc:RTCAble[RM], implicit val ecology:Ecology) 
  extends SpacePlugin(api, rtc) with ModelPersistence with IdentityPersistence with querki.types.ModelTypeDefiner with EcologyMember with AppsPure
{
  lazy val AccessControl = interface[querki.security.AccessControl]
  lazy val Apps = interface[Apps]
  lazy val AppsPersistence = interface[AppsPersistence]
  lazy val SpaceOps = interface[querki.spaces.SpaceOps]
  
  implicit def rm2rtc[A](rm:RM[A]) = rtc.toRTC(rm)
  
  /**
   * This will be called during the Actor's receive loop, and provides supplementary
   * handlers particular to Apps.
   */
  def receive:Actor.Receive = {
//    // Another Space is asking whether it may use this one as an App:
//    case SpacePluginMsg(req, _, RequestApp(ownerIdentity)) => {
//      // It is allowed if the owning Identity of the requesting Space has permission:
//      api.respond(AppRequestResponse(AccessControl.hasPermission(Apps.CanUseAsAppPerm, space.state, ownerIdentity, space.state)))
//    }
//    
//    // Another Space is trying to fetch this one's State, as an App: 
//    case SpacePluginMsg(req, _, FetchAppState(ownerIdentity)) => {
//      // Check that the owner of the *requesting* Space is allowed to do this in the first place:
//      if (AccessControl.hasPermission(Apps.CanUseAsAppPerm, space.state, ownerIdentity, space.state)) {
//        // TODO: this must become a chunked streaming protocol, with back-pressure and
//        // exactly-once semantics! Note that Akka Streaming is not yet good enough to handle
//        // this, since it doesn't yet work remotely.
//        val appSender = requester.context.actorOf(AppSender.props(ecology, sender, space.state))
//        appSender ! AppSender.Send
//      } else {
//        sender ! ThingError(new PublicException("Apps.notAnApp"))
//      }
//    }
//    

    // TBD: investigate the possible race condition, where a second AddApp request comes in while the first
    // one is processing.
    case SpacePluginMsg(req, _, AddApp(appId, appVersion)) => {
      // Belt-and-suspenders security check that the current user is allowed to do this. In theory, this
      // can only fail if something has changed significantly:
      if (!AccessControl.hasPermission(Apps.CanManipulateAppsPerm, space.state, req, space.state))
        throw new PublicException("Apps.notAllowed")
      
      // Okay -- load the app:
      api.loadAppVersion(appId, appVersion, api.state.allApps).map { app =>
        // Secondary check: is this App willing to be used?
        if (AccessControl.hasPermission(Apps.CanUseAsAppPerm, app, space.state.ownerIdentity.get.id, app)) {
          implicit val s = api.state
          val dhApp = dh(app)
          val dhParents = app.allApps.values.toSeq.map(dh(_))
          val msg = DHAddApp(req, DateTime.now, dhApp, dhParents)
          api.persistMsgThen(appId, msg, api.updateAfter(addFilledAppPure(app)))
        } else {
          // This App doesn't acknowledge that this person is allowed to use it:
          api.respond(ThingError(new PublicException("Apps.notAnApp")))
        }
      }
//      
//      // Okay -- is the specified Space willing to be used as an App?
//      (SpaceOps.spaceRegion ? SpacePluginMsg(req, appId, RequestApp(space.state.ownerIdentity.get.id))) map {
//        case AppRequestResponse(true) => {
//          // The App has accepted, so make the actual DB change.
//          // IMPORTANT: this is a slow operation! Should it be done here, or split into a separate worker?
//          AppsPersistence.addApp(space.state, appId)
//          // Tell the client that we're set:
//          sender ! ThingFound(appId, api.state)
//        }
//        case _ => sender ! ThingError(new PublicException("Apps.notAnApp"))
//      } 
    }
  }
}

/**
 * Check to see if this App will allow that Identity to use it.
 */
//private [apps] case class RequestApp(ownerIdentity:OID)

/**
 * Response to RequestApp.
 */
//private [apps] case class AppRequestResponse(allowed:Boolean)
//
///**
// * Sent from the Space to the App, to get its State. For now, this simply results in
// * CurrentState, but this will need to start a proper streaming protocol ASAP.
// */
//private [apps] case class FetchAppState(ownerIdentity:OID)

/**
 * Sent from AppsFunctions to the Plugin, so that app-adding can happen inside the Space's own context.
 */
private [apps] case class AddApp(appId:OID, appVersion:SpaceVersion)
