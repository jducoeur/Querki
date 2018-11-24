package querki.test.mid

import scala.concurrent.Promise

import upickle._
import autowire._

import play.api.mvc.{AnyContentAsFormUrlEncoded, Result, Session}
import play.api.test._
import play.api.test.Helpers._

import controllers.ClientController

import querki.api.{RequestMetadata, ResponseWrapper}
import querki.globals._

case class ClientContext(ownerId: String, spaceId: String)

/**
 * Interface layer for making calls to the Client API. Most tests will mix this in, but you
 * won't usually make the calls explicitly. Instead, call this via Autowire.
 */
trait ClientFuncs extends FormFuncs { self: MidTestBase =>
  private def controller = app.injector.instanceOf[ClientController]
  def clientController = controller
  
  private implicit lazy val materializer = app.materializer
  
  lazy val querkiVersion:String = querki.BuildInfo.version
  // TODO: we eventually want to be able to let tests populate this map, which becomes the metadata
  // sent in API calls. That is probably another variant of ClientBase?
  val currentPageParams: Map[String, String] = Map.empty
    
  trait ClientBase extends autowire.Client[String, upickle.default.Reader, upickle.default.Writer] {
    implicit def session: Session
    def callApi(req: FakeRequest[AnyContentAsFormUrlEncoded]): Future[Result]
    
    private val resultSessionPromise = Promise[Session]
    val resultSessionFut = resultSessionPromise.future
    
    def sendRequest(req: Request, metadata: RequestMetadata): Future[Result] = {
      val request = sessionFormRequest(
        "pickledRequest" -> write(req),
        "pickledMetadata" -> write(metadata)
      )
      
      callApi(request)
    }
  
    override def doCall(req: Request): Future[String] = {
      val metadata = RequestMetadata(querkiVersion, currentPageParams)
      for {
        result <- sendRequest(req, metadata)
        // Set the Session as a side-effect, since Autowire will squash it out:
        _ = resultSessionPromise.success(result.sess)
        // TODO: handle Exceptions from sendRequest -- see Client.scala
        // Note that most of the below code is adapted from play.api.test.Helpers, basically deconstructing
        // contentAsString() without all the Awaits:
        byteString <- result.body.consumeData
        charset = result.body.contentType match {
          case Some(s) if s.contains("charset=") => Some(s.split("; *charset=").drop(1).mkString.trim)
          case _ => None
        }
        response = byteString.decodeString(charset.getOrElse("utf-8"))
        wrapped = read[ResponseWrapper](response)
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
  
  class NSClient(implicit val session: Session) extends ClientBase {
    def callApi(request: FakeRequest[AnyContentAsFormUrlEncoded]): Future[Result] = {
      call(controller.rawApiRequest(), request)
    }
  }
  def withNsClient[R](body: ClientBase => R)(implicit session: Session): R = {
    body(new NSClient)
  }
  
  class Client(implicit ctx: ClientContext, val session: Session) extends ClientBase {
    def callApi(request: FakeRequest[AnyContentAsFormUrlEncoded]): Future[Result] = {
      call(controller.apiRequest(ctx.ownerId, ctx.spaceId), request)
    }
  }
  def withClient[R](body: ClientBase => R)(implicit ctx: ClientContext, session: Session): R = {
    body(new Client)
  }
}
