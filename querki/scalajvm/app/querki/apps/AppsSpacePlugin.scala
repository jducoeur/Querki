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
  lazy val Internal = interface[AppsInternal]
  
  def requester = spaceIn
  
  /**
   * This will be called during the Actor's receive loop, and provides supplementary
   * handlers particular to Apps.
   */
  def receive:Actor.Receive = {
    case SpacePluginMsg(req, _, FetchAppState(ownerIdentity)) => {
      // Check that the owner of the requesting Space is allowed to do this in the first place:
      if (AccessControl.hasPermission(Internal.CanUseAsAppProp, space.state, ownerIdentity, space.state)) {
        // TODO: this must become a chunked streaming protocol, with back-pressure and
        // exactly-once semantics! Note that Akka Streaming is not yet good enough to handle
        // this, since it doesn't yet work remotely.
        sender ! CurrentState(space.state)        
      } else {
        sender ! ThingError(new PublicException("Apps.notAnApp"))
      }
    }
  }
}

/**
 * Sent from the Space to the App, to get its State. For now, this simply results in
 * CurrentState, but this will need to start a proper streaming protocol ASAP.
 */
private [apps] case class FetchAppState(ownerIdentity:OID)
