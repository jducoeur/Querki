package querki.comm

import scala.concurrent.{Future, Promise}
import scala.scalajs.js
import scala.util.{Failure, Success, Try}

import org.querki.jquery.{JQueryAjaxSettings, JQueryDeferred, JQueryXHR}

import querki.globals._

/**
 * Provides access to the routing table.
 *
 * All entry points used by the Client (which in practice probably means all of them) should be
 * declared in the clientRoutes table in client.scala.html. Twirl turns that into proper Javascript
 * calls, and exposes them to the global namespace as "clientRoutes". This, in turn, picks those up
 * and provides them to the Client code as needed.
 *
 * @TODO: this isn't bad, but it's still mediocre -- since it's js.Dynamic, there's no static checking
 * of the calls at all. We'd like to do something that's truly strongly typed, but Autowire isn't the
 * solution to this particular problem, since it's fundamentally Play-based. The right solution is
 * probably at compile time, to build something that gets at the routing information *before* Scala.js,
 * reflects on that, and generates the client-side glue code.
 */
@js.native
@js.annotation.JSName("clientRoutes")
private[comm] object ClientRoutes extends js.Object {
  def controllers: js.Dynamic = js.native
}

class ApiCommEcot(e: Ecology) extends ClientEcot(e) with ApiComm {

  def implements = Set(classOf[ApiComm])

  /**
   * This Ecot exists primarily to expose this accessor in a managed (and stubbable) way.
   */
  lazy val controllers = ClientRoutes.controllers
}

/**
 * Represents a single Play entry point. You usually fetch this via your controllers, as in:
 * {{{
 * val call:PlayCall = ClientRoutes.controllers.MyController.MyCall(param1, param2)
 * }}}
 * Note that the type declaration is, sadly, necessary, in order to coerce the js.Dynamic result
 * from the controller into a PlayCall.
 *
 * Note that this type is dynamically generated by Play, via the javascriptRouter().
 */
@js.native
trait PlayCall extends js.Object {

  /**
   * Call this entry point with AJAX, using the default settings.
   */
  def ajax(): js.Dynamic = js.native

  /**
   * Call this AJAX entry point with the given jQuery settings.
   */
  def ajax(settings: JQueryAjaxSettings): js.Dynamic = js.native

  /**
   * The method of this entry point -- "GET", "POST" or whatever. Known in jQuery as "type".
   */
  def method: String = js.native

  /**
   * Synonym for method.
   */
  def `type`: String = js.native

  /**
   * The relative URL of this call.
   */
  def url: URL = js.native

  /**
   * The absolute URL of this call.
   */
  def absoluteURL: URL = js.native
}

sealed trait AjaxResult

case class AjaxSuccess(
  data: String,
  textStatus: String,
  jqXHR: JQueryDeferred
) extends AjaxResult

case class AjaxFailure(
  jqXHR: JQueryDeferred,
  textStatus: String,
  errorThrown: String
) extends AjaxResult

case class PlayAjaxException(
  jqXHR: JQueryXHR,
  textStatus: String,
  errorThrown: String
) extends Exception

/**
 * This wrapper around PlayCall exposes the Ajax stuff in a more Scala-idiomatic way, using Futures.
 * It deliberately apes dom.extensions.Ajax.
 */
class PlayAjax(call: PlayCall) {

  def callAjax(data: (String, String)*): Future[String] = {
    val promise = Promise[String]

    // Change this line to print all API calls.
    // TODO: this should come from config!
    val spewAPICalls = true

    // Note that this is *not* the literal dataStr,
    // mainly because that is fairly hard to read!
    if (spewAPICalls) println(s"Sending AJAX call ${data.map(pair => s"${pair._1}=${pair._2}").mkString("&")}")

    val dataStr = data.map { pair =>
      // TODO: in ScalaJS 0.6, encodeURIComponent has been moved to js.URIUtils:
      val encoded = js.URIUtils.encodeURIComponent(pair._2)
      s"${pair._1}=$encoded"
    }.mkString("&")
    val settings = JQueryAjaxSettings.data(dataStr)
    val deferred = call.ajax(settings).asInstanceOf[JQueryDeferred]
    deferred.done { (data: String, textStatus: String, jqXHR: JQueryDeferred) =>
      if (spewAPICalls) println(s"Got AJAX response $data")
      promise.success(data)
    }
    deferred.fail { (jqXHR: JQueryXHR, textStatus: String, errorThrown: String) =>
      println(
        s"Got AJAX error $errorThrown (${jqXHR.status}) from ${data.map(pair => s"${pair._1}=${pair._2}").mkString("&")} with ${jqXHR.responseText}"
      )
      promise.failure(PlayAjaxException(jqXHR, textStatus, errorThrown))
    }

    promise.future
  }

  def callAjax[U](cb: PartialFunction[AjaxResult, U]): Future[Try[U]] = {
    val promise = Promise[Try[U]]

    val deferred = call.ajax().asInstanceOf[JQueryDeferred]
    deferred.done { (data: String, textStatus: String, jqXHR: JQueryDeferred) =>
      promise.success(Success(cb(AjaxSuccess(data, textStatus, jqXHR))))
    }
    deferred.fail { (jqXHR: JQueryXHR, textStatus: String, errorThrown: String) =>
      promise.success(Failure({
        println(s"Got AJAX error $errorThrown (${jqXHR.status}) with ${jqXHR.responseText}")
        cb(AjaxFailure(jqXHR, textStatus, errorThrown))
        PlayAjaxException(jqXHR, textStatus, errorThrown)
      }))
    }

    promise.future
  }
}
