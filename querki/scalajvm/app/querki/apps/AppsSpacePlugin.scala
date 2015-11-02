package querki.apps

import akka.actor._

import org.querki.requester._

import models.Thing._

import querki.globals._
import querki.spaces._
import querki.spaces.messages._
import querki.util.PublicException
import querki.values.RequestContext

/**
 * This code runs *inside* the Space Actor, as a plugin. It has important constraints, as described in
 * SpaceAPI. It plays a bit fast and loose with those constraints, since it is doing such dramatic
 * violence to the Space.
 * 
 * @author jducoeur
 */
class AppsSpacePlugin(spaceIn:SpaceAPI, implicit val ecology:Ecology) extends SpacePlugin(spaceIn) with EcologyMember with RequesterImplicits
{
  lazy val AccessControl = interface[querki.security.AccessControl]
  lazy val Apps = interface[Apps]
  lazy val AppsPersistence = interface[AppsPersistence]
  lazy val Internal = interface[AppsInternal]
  lazy val SpaceOps = interface[querki.spaces.SpaceOps]
  
  def requester = spaceIn
  
  /**
   * This will be called during the Actor's receive loop, and provides supplementary
   * handlers particular to Apps.
   */
  def receive:Actor.Receive = {
    // Another Space is trying to fetch this one's State, as an App: 
    case SpacePluginMsg(req, _, FetchAppState(ownerIdentity)) => {
      // Check that the owner of the *requesting* Space is allowed to do this in the first place:
      if (AccessControl.hasPermission(Internal.CanUseAsAppProp, space.state, ownerIdentity, space.state)) {
        // TODO: this must become a chunked streaming protocol, with back-pressure and
        // exactly-once semantics! Note that Akka Streaming is not yet good enough to handle
        // this, since it doesn't yet work remotely.
        val appSender = requester.context.actorOf(AppSender.props(ecology, sender, space.state))
        appSender ! AppSender.Send
      } else {
        sender ! ThingError(new PublicException("Apps.notAnApp"))
      }
    }
    
    // TBD: investigate the possible race condition, where a second AddApp request comes in while the first
    // one is processing.
    case SpacePluginMsg(req, _, AddApp(appId)) => {
      // Belt-and-suspenders security check that the current user is allowed to do this:
      if (!AccessControl.hasPermission(Apps.CanManipulateAppsPerm, space.state, req, space.state))
        throw new PublicException("Apps.notAllowed")
      
      // Okay -- is the specified Space willing to be used as an App?
      (SpaceOps.spaceRegion ? SpacePluginMsg(req, appId, FetchAppState(space.state.ownerIdentity.get.id))) map {
        case CurrentState(appState) => {
          // The App has accepted, so make the actual DB change.
          // IMPORTANT: this is a slow operation! Should it be done here, or split into a separate worker?
          AppsPersistence.addApp(space.state, appId)
          // Tell the Space to reload itself, since the world has now changed significantly. This leaves the
          // Actor structure in place, but begins the process of reloading all the data.
          space.reloadSpace()
          // Okay, we're done:
          sender ! ThingFound(appId, appState)
        }
        case err:ThingError => sender ! err 
      } 
    }
  }
}

/**
 * Sent from the Space to the App, to get its State. For now, this simply results in
 * CurrentState, but this will need to start a proper streaming protocol ASAP.
 */
private [apps] case class FetchAppState(ownerIdentity:OID)

/**
 * Sent from AppsFunctions to the Plugin, so that app-adding can happen inside the Space's own context.
 */
private [apps] case class AddApp(appId:OID)
