package controllers

import upickle._

import models.{Thing, ThingId}

import querki.pages.PageIDs._

import querki.session.messages.{ClientError, ClientRequest, ClientResponse}
import querki.spaces.messages.{SessionRequest, ThingError}
import querki.spaces.messages.SpaceError._

class ClientController extends ApplicationBase {
  
  lazy val ClientApi = interface[querki.api.ClientApi]
  lazy val Tags = interface[querki.tags.Tags]
  
  def pageForThing(rc:PlayRequestContext, thing:Thing) = {
    Ok(views.html.client(rc, ThingPage, write(ClientApi.thingInfo(Some(thing), rc).get)))    
  }
  
  def thing(ownerId:String, spaceId:String, thingId:String) = withThing(false, ownerId, spaceId, thingId, Some({ 
    case (ThingError(error, stateOpt), rc) if (error.msgName == UnknownName) => {
      // We didn't find the requested Thing, so display a TagThing for it instead:
      val thing = Tags.getTag(thingId, stateOpt.get)
      val rcWithName = rc.copy(thing = Some(thing))
      pageForThing(rcWithName, thing)
    }
  })) { implicit rc =>
    pageForThing(rc, rc.thing.get)
  }

  // TODO: this shouldn't require withSpace! We should authenticate the user at this level, and then just
  // route directly to the UserSpaceSession. However, note that this is going to require some nasty surgery to
  // askSpaceMgr, which currently assumes that you have *already* resolved ownerId! Feh. But we have to fix it,
  // because withSpace deeply violates the long-run architecture -- we want to eventually *never* send the SpaceState
  // back to the Play layer.
  def apiRequest(ownerId:String, spaceId:String, apiId:Int, pickledRequest:String) = withRouting(ownerId) { implicit rc =>
    val request = read[autowire.Core.Request[String]](pickledRequest)
    askSpace(SessionRequest(rc.requesterOrAnon, rc.ownerId, ThingId(spaceId), ClientRequest(apiId, request, rc))) {
      case ClientResponse(pickled) => Ok(pickled)
      case ClientError(msg) => BadRequest(msg)
    }
  }
}
