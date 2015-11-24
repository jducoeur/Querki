package controllers

import scala.concurrent.duration._
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

import querki.api._
import querki.imexport.ImexportFunctions
import querki.pages.PageIDs._
import querki.session.messages.{MarcoPoloRequest, MarcoPoloResponse, SessionMessage}
import querki.spaces.messages.{SessionRequest, SpaceMgrMsg, ThingError}
import querki.spaces.messages.SpaceError._
import querki.streaming.UploadMessages._
import querki.util.PublicException

class ClientController extends ApplicationBase with StreamController {
  
  lazy val ClientApi = interface[querki.api.ClientApi]
  lazy val SystemManagement = interface[querki.system.SystemManagement]
  lazy val Tags = interface[querki.tags.Tags]
  
  def apiTrace = ApiInvocation.apiTrace _
  
  case class ApiRequest(pickledRequest:String, pickledMetadata:String)
  
  val requestForm = Form(
    mapping(
      "pickledRequest" -> nonEmptyText,
      "pickledMetadata" -> nonEmptyText
    )((pickledRequest, pickledMetadata) => ApiRequest(pickledRequest, pickledMetadata))
     (apiRequest => Some((apiRequest.pickledRequest, apiRequest.pickledMetadata)))
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
        if (rc.request.queryString.contains("_escaped_fragment_")) {
          val thingIdStr = rc.request.queryString("_escaped_fragment_").head
          Redirect(routes.RawController.thing(ownerId, spaceIdStr, thingIdStr))
        } else {
          Ok(views.html.client(rc, write(requestInfo)))
        }
      }
    } recoverWith {
      case pex:PublicException => doError(indexRoute, pex) 
    }
  }
  
  def thingRedirect(ownerId:String, spaceId:String, thingId:String) = Action {
    val spaceCall = routes.ClientController.space(ownerId, spaceId) 
    Redirect(new Call(spaceCall.method, spaceCall.url + s"#!$thingId"))
  }
  
  def unpickleRequest(rc:PlayRequestContext):(autowire.Core.Request[String], RequestMetadata) = {
    implicit val request = rc.request
    requestForm.bindFromRequest.fold(
      errors => throw new Exception("API got badly-defined request!"),
      apiRequest => (read[autowire.Core.Request[String]](apiRequest.pickledRequest), read[RequestMetadata](apiRequest.pickledMetadata))
    )
  }
  
  // NOTE: this generates a spurious error in Eclipse, because it's generated code.
  // Theoretically, we could get rid of this error as described in:
  //   https://github.com/sbt/sbt-buildinfo
  // But in practice that seems to screw up the client/server shared code.
  // TODO: figure out a way to suppress this error.
  def querkiVersion:String = querki.BuildInfo.version
  
  def apiRequestBase(prc:PlayRequestContext):Future[Result] = {
    val (req, metadata) = unpickleRequest(prc)
    if (metadata.version != querkiVersion) {
      apiTrace(s"apiRequest call to ${req.path}, but version is old")
      // Signal to the Client that it needs to reload
      // TODO: this will need to become more forgiving once this path is really being used as an
      // API. Maybe an additional Metadata flag saying whether being slightly out of date is okay?
      // Maybe a more sophisticated mechanism for tracking when the protocol has changed, and how?
      PreconditionFailed
    } else {
      apiTrace(s"apiRequest call to ${req.path}")
      val rc = prc.rc.copy(metadataOpt = Some(metadata))
      val request = ClientRequest(req, rc)
      if (ApiInvocation.requiresLogin(request) && rc.requester.isEmpty)
        BadRequest(write(new NotAllowedException()))
      else ApiInvocation.routeRequest(request) {
        case ClientResponse(pickled) => {
          apiTrace(s"    Call to ${req.path} resulted in $pickled")
          val userInfo = ClientApi.userInfo(prc.requester)
          val response = ResponseWrapper(userInfo, pickled)
          Ok(write(response))
        }
        case ClientError(msg) => {
          apiTrace(s"    Call to ${req.path} resulted in error $msg")
          BadRequest(msg)
        }
      }
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
