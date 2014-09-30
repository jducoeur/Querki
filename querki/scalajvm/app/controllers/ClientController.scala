package controllers

import upickle._

import models.{Thing, ThingId}

import querki.pages.{RenderedThing, ThingPageInfo}
import querki.pages.PageIDs._

import querki.session.messages.{ClientError, ClientRequest, ClientResponse}
import querki.spaces.messages.{SessionRequest, ThingError}
import querki.spaces.messages.SpaceError._

class ClientController extends ApplicationBase {
  
  lazy val ClientApi = interface[querki.api.ClientApi]
  lazy val Tags = interface[querki.tags.Tags]
  
  def pageForThing(rc:PlayRequestContext, thing:Thing) = {
    val rendered = thing.render(rc).display.toString
    Ok(views.html.client(rc, ThingPage, write(ThingPageInfo(ClientApi.thingInfo(Some(thing), rc).get, rendered))))    
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
  
  def renderThing(ownerId:String, spaceId:String, thingId:String) = withThing(false, ownerId, spaceId, thingId) { implicit rc =>
    val thing = rc.thing.get
    Ok(write(RenderedThing(thing.render(rc).display.toString)))
  }

  // TODO: this shouldn't require withSpace! We should authenticate the user at this level, and then just
  // route directly to the UserSpaceSession. However, note that this is going to require some nasty surgery to
  // askSpaceMgr, which currently assumes that you have *already* resolved ownerId! Feh. But we have to fix it,
  // because withSpace deeply violates the long-run architecture -- we want to eventually *never* send the SpaceState
  // back to the Play layer.
  def apiRequest(ownerId:String, spaceId:String, apiId:Int, path:String, pickledArgs:String) = withSpace(false, ownerId, spaceId) { implicit rc =>
    val args = read[Map[String,String]](pickledArgs)
    askSpace(SessionRequest(rc.requesterOrAnon, rc.ownerId, ThingId(spaceId), ClientRequest(apiId, path.split('/'), args, rc))) {
      case ClientResponse(pickled) => Ok(pickled)
      case ClientError(msg) => BadRequest(msg)
    }
  }
}
