package controllers

import scala.concurrent.Future

import upickle._
import autowire._

import models.{Thing, ThingId}

import querki.global._
import Implicits.execContext

import querki.api.{ClientApis, ThingFunctions}
import querki.pages.PageIDs._
import querki.session.messages.{ClientError, ClientRequest, ClientResponse}
import querki.spaces.messages.{SessionRequest, SpaceMgrMsg, ThingError}
import querki.spaces.messages.SpaceError._

class ClientController extends ApplicationBase {
  
  lazy val ClientApi = interface[querki.api.ClientApi]
  lazy val Tags = interface[querki.tags.Tags]
  
  def thing(ownerId:String, spaceId:String, thingId:String) = withRouting(ownerId) { implicit rc =>
    class LocalClient extends autowire.Client[String, upickle.Reader, upickle.Writer] {
	  override def doCall(req: Request): Future[String] = {
	    SpaceOps.askSpaceManager2(SessionRequest(rc.requesterOrAnon, rc.ownerId, ThingId(spaceId), ClientRequest(ClientApis.ThingFunctionsId, req, rc))) {
	      case ClientResponse(pickled) => Future.successful(pickled)
	      case ClientError(msg) => Future.failed(new Exception(msg))
	    }
	  }
	
	  def read[Result: upickle.Reader](p: String) = upickle.read[Result](p)
	  def write[Result: upickle.Writer](r: Result) = upickle.write(r)
	}
    val client = new LocalClient
    
    client[ThingFunctions].getThingInfo(thingId).call().map { requestInfo =>
      Ok(views.html.client(rc, ThingPage, write(requestInfo)))    
    }
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
