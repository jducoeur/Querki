package controllers

import scala.concurrent.duration._
import scala.concurrent.Future
import akka.util.Timeout

import upickle._
import autowire._

import models.{Thing, ThingId}

import querki.globals._
import Implicits.execContext

import querki.api.ThingFunctions
import querki.pages.PageIDs._
import querki.session.UserSessionMessages.{UserSessionClientRequest, UserSessionMsg}
import querki.session.messages._
import querki.spaces.messages.{SessionRequest, SpaceMgrMsg, ThingError}
import querki.spaces.messages.SpaceError._

class ClientController extends ApplicationBase {
  
  lazy val ClientApi = interface[querki.api.ClientApi]
  lazy val Tags = interface[querki.tags.Tags]
  
  /**
   * Send the given request to the UserSpaceSession, and do the given callback when it responds.
   * 
   * TODO: the client should be keeping track of the path to the UserSpaceSession somehow -- maybe in a cookie -- and
   * we should be trying that direct path first, instead of routing everything through the SpaceManager every time.
   * The more we can reduce the SpaceManager as a bottleneck, the better.
   */
  def askUserSpaceSession[B](rc:PlayRequestContext, msg:SessionMessage)(cb: PartialFunction[Any, Future[B]]):Future[B] = {
    SpaceOps.askSpaceManager2(SessionRequest(rc.requesterOrAnon, rc.ownerId, ThingId(rc.spaceIdOpt.get), msg))(cb)
  }
  
  def askUserSession[B](rc:PlayRequestContext, msg:UserSessionMsg)(cb: PartialFunction[Any, Future[B]]):Future[B] = {
    akka.pattern.ask(UserSessionMgr.sessionManager, msg)(Timeout(5 seconds)).flatMap(cb)
  }

  /**
   * Allows purely server-side code to invoke Session functions, the same way the Client does.
   */
  class LocalClient(rc:PlayRequestContext) extends autowire.Client[String, upickle.Reader, upickle.Writer] {
	override def doCall(req: Request): Future[String] = {
	  askUserSpaceSession(rc, ClientRequest(req, rc)) {
	    case ClientResponse(pickled) => Future.successful(pickled)
	    case ClientError(msg) => Future.failed(new Exception(msg))
	  }
	}
	
	def read[Result: upickle.Reader](p: String) = upickle.read[Result](p)
	def write[Result: upickle.Writer](r: Result) = upickle.write(r)
  }
  
  def space(ownerId:String, spaceId:String) = withRouting(ownerId, spaceId) { implicit rc =>
    val client = new LocalClient(rc)
    
    client[ThingFunctions].getRequestInfo().call().map { requestInfo =>
      Ok(views.html.client(rc, write(requestInfo)))
    }
  }
  
  def apiRequest(ownerId:String, spaceId:String, pickledRequest:String) = withRouting(ownerId, spaceId) { implicit rc =>
    val request = read[autowire.Core.Request[String]](pickledRequest)
    askUserSpaceSession(rc, ClientRequest(request, rc)) {
      case ClientResponse(pickled) => Ok(pickled)
      case ClientError(msg) => BadRequest(msg)
    }
  }
  
  def userApiRequest(pickledRequest:String) = withUser(true) { implicit rc =>
    val request = read[autowire.Core.Request[String]](pickledRequest)
    askUserSession(rc, UserSessionClientRequest(rc.requesterOrAnon.id, ClientRequest(request, rc))) {
      case ClientResponse(pickled) => Ok(pickled)
      case ClientError(msg) => BadRequest(msg)
    }
  }
  
  def commonApiRequest(pickledRequest:String) = withUser(false) { implicit rc =>
    val request = read[autowire.Core.Request[String]](pickledRequest)
    ClientApi.handleCommonFunction(request).map { 
      case ClientResponse(pickled) => Ok(pickled)
      case ClientError(msg) => BadRequest(msg)
    }
  }

  /**
   * Serves out requests from MarcoPolo on the client side.
   * 
   * @param propId (ThingId) The Property that the user is trying to add new values to.
   */
  def marcoPolo(ownerId:String, spaceId:String, propIdStr:String) = withRouting(ownerId, spaceId) { implicit rc =>
    // Note that q is intentionally *not* in the signature above. This is so that we can hand Manifest a URL that it can
    // then tack &q= on to.
    val q = rc.queryParam("q").head
    val propId =
      if (propIdStr.length > 0)
        Some(ThingId(propIdStr))
      else
        None
    askUserSpaceSession(rc, MarcoPoloRequest(propId, q)) {
      case MarcoPoloResponse(items) => {
        Ok(write(items))
      }
      case _ => BadRequest("Couldn't parse items.")
    }
  }
}
