package controllers

import scala.concurrent.duration._
import scala.concurrent.Future
import akka.util.Timeout

import play.api.data._
import play.api.data.Forms._
import play.api.mvc.{Action, Call}

import upickle._
import autowire._

import models.{AsName, AsOID, Thing, ThingId}

import querki.globals._
import Implicits.execContext

import querki.api.ThingFunctions
import querki.pages.PageIDs._
import querki.session.UserSessionMessages.{UserSessionClientRequest, UserSessionMsg}
import querki.session.messages._
import querki.spaces.messages.{SessionRequest, SpaceMgrMsg, ThingError}
import querki.spaces.messages.SpaceError._
import querki.util.PublicException

class ClientController extends ApplicationBase {
  
  lazy val ClientApi = interface[querki.api.ClientApi]
  lazy val Tags = interface[querki.tags.Tags]
  
  val requestForm = Form(
    mapping(
      "pickledRequest" -> nonEmptyText
    )((pickledRequest) => pickledRequest)
     ((pickledRequest:String) => Some(pickledRequest))
  )  
  
  /**
   * Send the given request to the UserSpaceSession, and do the given callback when it responds.
   * 
   * TODO: the client should be keeping track of the path to the UserSpaceSession somehow -- maybe in a cookie -- and
   * we should be trying that direct path first, instead of routing everything through the SpaceManager every time.
   * The more we can reduce the SpaceManager as a bottleneck, the better.
   */
  def askUserSpaceSession[B](rc:PlayRequestContext, msg:SessionMessage)(cb: PartialFunction[Any, Future[B]]):Future[B] = {
    val spaceId = ThingId(rc.spaceIdOpt.get) match {
      case AsOID(id) => id
      case AsName(name) => throw new Exception(s"Trying to send message $msg, but only have Space name $name!")
    } 
    SpaceOps.askSpace2(SessionRequest(rc.requesterOrAnon, spaceId, msg))(cb)
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
	    case ThingError(pex, _) => Future.failed(pex)
	  }
	}
	
	def read[Result: upickle.Reader](p: String) = upickle.read[Result](p)
	def write[Result: upickle.Writer](r: Result) = upickle.write(r)
  }
  
  def space(ownerId:String, spaceIdStr:String) = withRouting(ownerId, spaceIdStr) { implicit rawRc =>
    // Unlike the API calls, we have to assume we have a name-style ThingId here:
    SpaceOps.getSpaceId(rawRc.ownerId, spaceIdStr).flatMap { spaceId =>
      val rc = rawRc.copy(spaceIdOpt = Some(spaceId.toThingId))
      val client = new LocalClient(rc)
      
      client[ThingFunctions].getRequestInfo().call().map { requestInfo =>
        if (requestInfo.forbidden) {
          unknownSpace(spaceIdStr)
        } else {
          Ok(views.html.client(rc, write(requestInfo)))
        }
      } recoverWith {
        case pex:PublicException => doError(routes.Application.index, pex) 
      }
    }
  }
  
  def thingRedirect(ownerId:String, spaceId:String, thingId:String) = Action {
    val spaceCall = routes.ClientController.space(ownerId, spaceId) 
    Redirect(new Call(spaceCall.method, spaceCall.url + s"#$thingId"))
  }
  
  def unpickleRequest(rc:PlayRequestContext):autowire.Core.Request[String] = {
    implicit val request = rc.request
    requestForm.bindFromRequest.fold(
      errors => throw new Exception("API got badly-defined request!"),
      pickledRequest => read[autowire.Core.Request[String]](pickledRequest)
    )
  }
  
  def apiRequest(ownerId:String, spaceId:String) = withRouting(ownerId, spaceId) { implicit rc =>
    val request = unpickleRequest(rc)
    askUserSpaceSession(rc, ClientRequest(request, rc)) {
      case ClientResponse(pickled) => Ok(pickled)
      case ClientError(msg) => BadRequest(msg)
    }
  }
  
  // NOTE: this calls withUser(false) because we do *not* want the default behavior if this is
  // an anonymous request -- that redirects to index, and (worse) sets the returnTo to here, which
  // produces silly nonsense when you next log in. So we handle Anonymous requests ourselves here.
  def userApiRequest = withUser(false) { implicit rc =>
    rc.requester match {
      case Some(requester) => {
	    val request = unpickleRequest(rc)
	    askUserSession(rc, UserSessionClientRequest(rc.requesterOrAnon.id, ClientRequest(request, rc))) {
	      case ClientResponse(pickled) => Ok(pickled)
	      case ClientError(msg) => BadRequest(msg)
	    }        
      }
      case None => BadRequest("Not logged in")
    }
  }
  
  def commonApiRequest = withUser(false) { implicit rc =>
    val request = unpickleRequest(rc)
    ClientApi.handleCommonFunction(rc, request).map { 
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
    askUserSpaceSession(rc, MarcoPoloRequest(propId, q, rc)) {
      case MarcoPoloResponse(items) => {
        Ok(write(items))
      }
      case _ => BadRequest("Couldn't parse items.")
    }
  }
}
