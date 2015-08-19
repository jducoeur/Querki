package controllers

import scala.concurrent.duration._
import scala.concurrent.Future
import akka.actor._
import akka.pattern._
import akka.util.Timeout

import play.api.data._
import play.api.data.Forms._
import play.api.mvc._

import upickle._
import autowire._

import models.{AsName, AsOID, MIMEType, Thing, ThingId}

import querki.globals._
import Implicits.execContext

import querki.api._
import querki.imexport.ImexportFunctions
import querki.pages.PageIDs._
import querki.session.messages.{MarcoPoloRequest, MarcoPoloResponse, SessionMessage}
import querki.spaces.messages.{SessionRequest, SpaceMgrMsg, ThingError}
import querki.spaces.messages.SpaceError._
import querki.streaming.UploadMessages._
import querki.util.PublicException

class ClientController extends ApplicationBase with StreamController {
  
  lazy val ApiInvocation = interface[querki.api.ApiInvocation]
  lazy val ClientApi = interface[querki.api.ClientApi]
  lazy val SystemManagement = interface[querki.system.SystemManagement]
  lazy val Tags = interface[querki.tags.Tags]
  
  val requestForm = Form(
    mapping(
      "pickledRequest" -> nonEmptyText
    )((pickledRequest) => pickledRequest)
     ((pickledRequest:String) => Some(pickledRequest))
  )  
  
  /**
   * Send the given request to the UserSpaceSession, and do the given callback when it responds.
   */
  def askUserSpaceSession[B](rc:PlayRequestContext, msg:SessionMessage)(cb: PartialFunction[Any, Future[B]]):Future[B] = {
    val spaceId = ThingId(rc.spaceIdOpt.get) match {
      case AsOID(id) => id
      case AsName(name) => throw new Exception(s"Trying to send message $msg, but only have Space name $name!")
    } 
    SpaceOps.askSpace2(SessionRequest(rc.requesterOrAnon, spaceId, msg))(cb)
  }
  
  def askUserSession[B](rc:PlayRequestContext, msg:ClientRequest)(cb: PartialFunction[Any, Future[B]]):Future[B] = {
    akka.pattern.ask(UserSessionMgr.sessionManager, msg)(Timeout(5 seconds)).flatMap(cb)
  }

  /**
   * Allows purely server-side code to invoke Session functions, the same way the Client does.
   */
  class LocalClient(rc:PlayRequestContext) extends autowire.Client[String, upickle.Reader, upickle.Writer] {
  	override def doCall(req: Request): Future[String] = {
      ApiInvocation.routeRequest(ClientRequest(req, rc)) {
        case ClientResponse(pickled) => Future.successful(pickled)
  	    case ClientError(msg) => Future.failed(new Exception(msg))
  	    case ThingError(pex, _) => Future.failed(pex)
  	  }
  	}
  	
  	def read[Result: upickle.Reader](p: String) = upickle.read[Result](p)
  	def write[Result: upickle.Writer](r: Result) = upickle.write(r)
  }
  
  def withLocalClient(ownerId:String, spaceIdStr:String)(cb:(PlayRequestContext, LocalClient) => Future[Result]) = 
    withRouting(ownerId, spaceIdStr) 
  { implicit rawRc =>
    // Unlike the API calls, we have to assume we have a name-style ThingId here:
    SpaceOps.getSpaceId(rawRc.ownerId, spaceIdStr).flatMap { spaceId =>
      val rc = rawRc.copy(spaceIdOpt = Some(spaceId.toThingId))
      val client = new LocalClient(rc)
      
      cb(rc, client)
    }
  }
  
  def index = withUser(false) { rc =>
    rc.requester match {
      case Some(requester) => {
        val requestInfo = ClientApi.rootRequestInfo(rc)
        Ok(views.html.client(rc, write(requestInfo)))
      }
      // For the moment, in the not-logged-in case, we still show the old root page:
      case _ => Ok(views.html.index(this, rc))
    }
  } 
  
  def space(ownerId:String, spaceIdStr:String) = withLocalClient(ownerId, spaceIdStr) { (rc, client) =>
    implicit val r = rc
    client[ThingFunctions].getRequestInfo().call().map { requestInfo =>
      if (requestInfo.forbidden) {
        unknownSpace(spaceIdStr)
      } else {
        Ok(views.html.client(rc, write(requestInfo)))
      }
    } recoverWith {
      case pex:PublicException => doError(indexRoute, pex) 
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
  
  def apiRequestBase(rc:PlayRequestContext):Future[Result] = {
    val request = ClientRequest(unpickleRequest(rc), rc)
    if (ApiInvocation.requiresLogin(request) && rc.requester.isEmpty)
      BadRequest(write(new NotAllowedException()))
    else ApiInvocation.routeRequest(request) {
      case ClientResponse(pickled) => Ok(pickled)
      case ClientError(msg) => BadRequest(msg)
    }    
  }
  
  def apiRequest(ownerId:String, spaceId:String) = withRouting(ownerId, spaceId) { rc =>
    apiRequestBase(rc)
  }
  
  // Entry point for the Client to use when it isn't under a Space, or is just starting up:
  def rawApiRequest = withUser(false) { rc =>
    apiRequestBase(rc)
  }
  
  /**
   * This isn't an entry point, it's the BodyParser for upload(). It expects to receive the path
   * to an UploadActor; it finds that Actor, then sends it the chunks as they come in. 
   */
  private def uploadReceiver(targetActorPath:String)(rh:RequestHeader) = {
    def produceUploadLocation:Future[ActorRef] = {
      // Fairly short timeout for the identification procedure:
      implicit val timeout = Timeout(5 seconds)
      val selection = SystemManagement.actorSystem.actorSelection(targetActorPath)
      for {
        ActorIdentity(_, refOpt) <- selection ? Identify("dummy")
      }
        // TODO: return a decent error to the client if the Actor isn't found:
        yield refOpt.get
    }
    
    uploadBodyChunks(produceUploadLocation)(rh)
  }

  /**
   * The generic upload() entry point. This expects that you have already called an API function that
   * creates an UploadActor of the appropriate type, and returns its path. You then pass that path
   * into here to upload the actual data, and the UploadActor should process the data once it receives
   * the UploadComplete signal.
   */
  def upload(targetActorPath:String) = 
    withUser(true, parser = BodyParser(uploadReceiver(targetActorPath) _)) 
  { rc =>
    for {
      uploadRef <- rc.request.body.asInstanceOf[Future[ActorRef]]
      // We ask the UploadActor how much time it's going to need to process this upload:
      // TODO: this mechanism is now obsolete; the architecture is more interactive, and
      // UploadComplete responds quickly:
      UploadTimeout(uploadDuration) <- uploadRef.ask(GetUploadTimeout)(5 seconds)
      uploadTimeout = Timeout(uploadDuration)
      result <- uploadRef.ask(UploadComplete(rc))(uploadTimeout)
    }
      yield {
        result match {
          case UploadProcessSuccessful(response) => Ok(response)
          case UploadProcessFailed(ex) => BadRequest(ex)
          case _ => { QLog.error(s"upload() entry point got unknown response $result"); InternalServerError("upload() got a bad response internally") }
        }
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
  
  def exportSpace(ownerId:String, spaceId:String) = withLocalClient(ownerId, spaceId) { (rc, client) =>
    implicit val r = rc
    client[ImexportFunctions].exportSpace().call() map { exported =>
      Ok(exported).as(MIMEType.XML)
    } recoverWith {
      case pex:PublicException => doError(indexRoute, pex) 
    }
  }
}
