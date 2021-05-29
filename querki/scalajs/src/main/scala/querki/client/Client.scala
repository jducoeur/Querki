package querki.client

import scala.concurrent.Future
import upickle.default._
import querki.globals._
import querki.api._
import querki.comm._
import querki.history.HistoryFunctions

class ClientImpl(e: Ecology) extends ClientEcot(e) with Client {

  def implements = Set(classOf[Client])

  lazy val controllers = interface[querki.comm.ApiComm].controllers
  lazy val DataAccess = interface[querki.data.DataAccess]
  lazy val History = interface[querki.history.History]
  lazy val PageManager = interface[querki.display.PageManager]
  lazy val StatusLine = interface[querki.display.StatusLine]
  lazy val UserAccess = interface[querki.identity.UserAccess]

  def translateServerException(ex: Throwable): Throwable = {
    ex match {
      case ex @ PlayAjaxException(jqXHR, textStatus, errorThrown) => {
        if (jqXHR.status == 412) {
          // The server sent a PreconditionFailed, which is the signal that the Client
          // is out of date and the protocols may be inconsistent. So force a hard refresh.
          // IMPORTANT: I suspect this doesn't return anything; we're tearing down the world
          // and starting again.
          PageManager.fullReload()
          ex
        } else if (jqXHR.status == 504) {
          // We got a Gateway Timeout, which basically means that the server took too long to respond, period. So there
          // is no meaningful content here.
          StatusLine.showUntilChange(
            s"The page took too long to display, and has failed. It may need simplifying to display in less time."
          )
          ex
        } else if (jqXHR.status >= 500) {
          StatusLine.showUntilChange(
            s"There was an internal error (code ${jqXHR.status})! Sorry; it has been logged. If this persists, please tell us."
          )
          ex
        } else {
          try {
            read[ApiException](jqXHR.responseText)
          } catch {
            // The normal case -- the server sent an ApiException, which we will propagate up
            // to the calling code:
            case aex: querki.api.ApiException => aex
            // The server sent a non-ApiException, which is unfortunate. Just display it:
            case _: Throwable => {
              StatusLine.showUntilChange(jqXHR.responseText)
              ex
            }
          }
        }
      }
      case _: Throwable => {
        // Well, that's not good.
        // TODO: should we have some mechanism to propagate this exception back to the server,
        // and log it? Probably...
        println(s"Client.interceptFailures somehow got non-PlayAjaxException $ex")
        ex
      }
    }
  }

  def interceptFailures(caller: => Future[String]): Future[String] = {
    caller.transform(
      { response =>
        val wrapped = read[ResponseWrapper](response)
        UserAccess.setUser(wrapped.currentUser)
        wrapped.payload
      },
      { ex => translateServerException(ex) }
    )
  }

  def makeCall(
    req: Request,
    ajax: PlayAjax
  ): Future[String] = {
    if (History.viewingHistory && !History.isLegalDuringHistory(req))
      Future.failed(new Exception("You're not allowed to do that while viewing History"))
    else {
      val params =
        if (History.viewingHistory)
          PageManager.currentPageParams + (HistoryFunctions.viewingHistoryParam -> History.currentHistoryVersion.get.toString)
        else
          PageManager.currentPageParams
      val metadata = RequestMetadata(DataAccess.querkiVersion, params)
      interceptFailures(ajax.callAjax("pickledRequest" -> write(req), "pickledMetadata" -> write(metadata)))
    }
  }

  override def doCall(req: Request): Future[String] = {
    try {
      if (DataAccess.space.isEmpty) {
        // We're not started, or not under the aegis of a Space, so there is no
        // owner/space pair to use. Hopefully this request understands that.
        // TODO: can we put something in the API definition to enforce this? That is,
        // so that we can say "this request requires a Space"? Probably -- figure it out...
        makeCall(req, controllers.ClientController.rawApiRequest())
      } else {
        makeCall(
          req,
          controllers.ClientController.apiRequest(
            DataAccess.userName,
            DataAccess.spaceId.underlying
          )
        )
      }
    } catch {
      // Note that we need to catch and report exceptions here; otherwise, they tend to get
      // lost inside Autowire:
      case (ex: Exception) => {
        println(s"Got exception in doCall: ${ex.getMessage()}")
        throw ex
      }
    }
  }

  def read[Result : upickle.default.Reader](p: String) = {
    try {
      upickle.default.read[Result](p)
    } catch {
      case ex: Exception => {
        println(s"Exception while trying to unpickle response $p: $ex")
        throw ex
      }
    }
  }
  def write[Result : upickle.default.Writer](r: Result) = upickle.default.write(r)
}
