package querki.test.mid

import scala.concurrent.Promise
import upickle.default.{ReadWriter => RW, _}
import play.api.mvc.{AnyContentAsFormUrlEncoded, Result, Session}
import play.api.test._
import play.api.test.Helpers._
import controllers.ClientController
import querki.api._
import querki.data._
import querki.globals._
import AllFuncs._
import akka.stream.Materializer
import org.scalactic.source.Position

/**
 * Interface layer for making calls to the Client API. Most tests will mix this in, but you
 * won't usually make the calls explicitly. Instead, call this via Autowire.
 */
trait ClientFuncs {
  implicit lazy val clientFuncs: ClientFuncs = this

  lazy val querkiVersion: String = querki.BuildInfo.version

  implicit val requestRW: RW[autowire.Core.Request[String]] = macroRW

  trait ClientBase extends autowire.Client[String, upickle.default.Reader, upickle.default.Writer] {
    implicit def session: Session
    def harness: HarnessInfo
    def callApi(req: FakeRequest[AnyContentAsFormUrlEncoded]): Future[Result]
    def currentPageParams: Map[String, String]

    implicit lazy val materializer: Materializer = harness.app.materializer
    def controller = harness.controller[ClientController]

    private val resultPromise = Promise[Result]
    val resultFut = resultPromise.future

    def sendRequest(
      req: Request,
      metadata: RequestMetadata
    ): Future[Result] = {
      val request = sessionFormRequest(
        "pickledRequest" -> write(req),
        "pickledMetadata" -> write(metadata)
      )

      callApi(request)
    }

    def translateException(
      response: String,
      req: Request
    )(implicit
      pos: Position
    ): Nothing = {
      try {
        val aex = read[ApiException](response)
        throw aex
      } catch {
        // The normal case -- the server sent an ApiException, which we will propagate up
        // to the calling code:
        case aex: querki.api.ApiException => throw aex
        // The server sent a non-ApiException, which is unfortunate. Just display it:
        case _: Throwable => {
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
          case _                                 => None
        }
        response = byteString.decodeString(charset.getOrElse("utf-8"))
        wrapped =
          readIfMatches[ResponseWrapper](response, "currentUser", "payload") match {
            case Some(result) => result
            case None         => translateException(response, req)
          }
        // TODO: we need to do something akin to this. How do we do so from inside here, without being
        // all horribly mutable? Maybe we should be taking the current User as a parameter, and asserting
        // that in the normal case? Should the result ever change outside of login/logout?
        // UserAccess.setUser(wrapped.currentUser)
      } yield wrapped.payload
    }

    /**
     * Read the specified type from the given String.
     *
     * This will throw an Exception *and print an error message* if the value isn't valid. So if you're
     * not sure, use readOptional() instead.
     */
    def read[Result : upickle.default.Reader](p: String): Result = {
      try {
        upickle.default.read[Result](p)
      } catch {
        case ex: Exception => {
          println(s"Exception while trying to unpickle response $p: $ex")
          throw ex
        }
      }
    }

    /**
     * A tolerant version of read(), that lets you specify a set of required fields and simply returns None if
     * they aren't present, instead of throwing.
     *
     * Really, we want a better "validate" function here instead. If later versions of upickle add that, switch
     * to using that instead. But we want *something* so that doCall() doesn't print errors every time we
     * receive an ApiException.
     */
    def readIfMatches[Result : upickle.default.Reader](
      p: String,
      expectedFields: String*
    ): Option[Result] = {
      val js = ujson.read(p)
      val fieldMap = js.obj
      if (expectedFields.forall(fieldMap.contains)) {
        try {
          Some(upickle.default.read[Result](js))
        } catch {
          case ex: Exception => {
            println(s"Exception while trying to unpickle response $p: $ex")
            throw ex
          }
        }
      } else
        None
    }

    def write[Result : upickle.default.Writer](r: Result) = upickle.default.write(r)
  }

  class NSClient(
    val harness: HarnessInfo,
    val session: Session,
    val currentPageParams: Map[String, String]
  ) extends ClientBase {

    def callApi(request: FakeRequest[AnyContentAsFormUrlEncoded]): Future[Result] = {
      call(controller.rawApiRequest(), request).map(_.bakeCookies())
    }
  }

  class Client(
    val harness: HarnessInfo,
    spaceInfo: SpaceInfo,
    val session: Session,
    val currentPageParams: Map[String, String]
  ) extends ClientBase {

    def callApi(request: FakeRequest[AnyContentAsFormUrlEncoded]): Future[Result] = {
      call(controller.apiRequest(spaceInfo.ownerHandle, spaceInfo.oid.underlying), request).map(_.bakeCookies())
    }
  }
}

object ClientFuncs extends ClientFuncs
