package querki.test.mid

import scala.concurrent.Promise

import upickle._
import autowire._

import play.api.mvc.{AnyContentAsFormUrlEncoded, Result, Session}
import play.api.test._
import play.api.test.Helpers._

import controllers.ClientController

import querki.api._
import querki.data._
import querki.globals._

import AllFuncs._

/**
 * Interface layer for making calls to the Client API. Most tests will mix this in, but you
 * won't usually make the calls explicitly. Instead, call this via Autowire.
 */
trait ClientFuncs {
  implicit lazy val clientFuncs = this
  
  lazy val querkiVersion:String = querki.BuildInfo.version
  // TODO: we eventually want to be able to let tests populate this map, which becomes the metadata
  // sent in API calls. That is probably another variant of ClientBase?
  val currentPageParams: Map[String, String] = Map.empty
    
  trait ClientBase extends autowire.Client[String, upickle.default.Reader, upickle.default.Writer] {
    implicit def session: Session
    def harness: HarnessInfo
    def callApi(req: FakeRequest[AnyContentAsFormUrlEncoded]): Future[Result]
    
    implicit lazy val materializer = harness.app.materializer
    def controller = harness.controller[ClientController]
    
    private val resultPromise = Promise[Result]
    val resultFut = resultPromise.future
    
    def sendRequest(req: Request, metadata: RequestMetadata): Future[Result] = {
      val request = sessionFormRequest(
        "pickledRequest" -> write(req),
        "pickledMetadata" -> write(metadata)
      )
      
      callApi(request)
    }
    
    def translateException(response: String, req: Request): Nothing = {
      try {
        val aex = read[ApiException](response)
        throw aex
      } catch {
        // The normal case -- the server sent an ApiException, which we will propagate up
        // to the calling code:
        case aex:querki.api.ApiException => throw aex
        // The server sent a non-ApiException, which is unfortunate. Just display it:
        case _:Throwable => {
          if (response.startsWith("Unexpected error."))
            throw new Exception(s"Got unexpected error from request $req")
          throw new Exception(s"Unable to parse server response $response from request $req")
        }
      }
    }
  
    override def doCall(req: Request): Future[String] = {
      val metadata = RequestMetadata(querkiVersion, currentPageParams)
      for {
        result <- sendRequest(req, metadata)
        // Set the Result as a side-effect, since Autowire will squash it out:
        _ = resultPromise.success(result)
        // TODO: handle Exceptions from sendRequest -- see Client.scala
        // Note that most of the below code is adapted from play.api.test.Helpers, basically deconstructing
        // contentAsString() without all the Awaits:
        byteString <- result.body.consumeData
        charset = result.body.contentType match {
          case Some(s) if s.contains("charset=") => Some(s.split("; *charset=").drop(1).mkString.trim)
          case _ => None
        }
        response = byteString.decodeString(charset.getOrElse("utf-8"))
        wrapped = try {
          read[ResponseWrapper](response)
        } catch {
          case t: Throwable => translateException(response, req)
        }
        // TODO: we need to do something akin to this. How do we do so from inside here, without being
        // all horribly mutable? Maybe we should be taking the current User as a parameter, and asserting
        // that in the normal case? Should the result ever change outside of login/logout?
        // UserAccess.setUser(wrapped.currentUser)
      }
        yield wrapped.payload
    }
    
    def read[Result: upickle.default.Reader](p: String) = {
      try {
        upickle.default.read[Result](p)
      } catch {
        case ex:Exception => {
          println(s"Exception while trying to unpickle response $p: $ex")
          throw ex
        }
      }
    }
    def write[Result: upickle.default.Writer](r: Result) = upickle.default.write(r)
  }
  
  class NSClient(val harness: HarnessInfo, val session: Session) extends ClientBase {
    def callApi(request: FakeRequest[AnyContentAsFormUrlEncoded]): Future[Result] = {
      call(controller.rawApiRequest(), request)
    }
  }
  
  class Client(val harness: HarnessInfo, spaceInfo: SpaceInfo, val session: Session) extends ClientBase {
    def callApi(request: FakeRequest[AnyContentAsFormUrlEncoded]): Future[Result] = {
      call(controller.apiRequest(spaceInfo.ownerHandle, spaceInfo.oid.underlying), request)
    }
  }
}

object ClientFuncs extends ClientFuncs
