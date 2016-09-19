package querki.api

import java.lang.reflect.Constructor

import scala.concurrent.duration._
import scala.reflect.ClassTag

import akka.actor.ActorRef
import akka.pattern._
import akka.util.Timeout

import querki.ecology._
import querki.globals._
import querki.streaming._

/**
 * @author jducoeur
 */
class ApiManagement(e:Ecology) extends QuerkiEcot(e) with ApiRegistry with ApiInvocation {
  
  lazy val History = interface[querki.history.History]
  lazy val SystemManagement = interface[querki.system.SystemManagement]
  lazy val actorSystem = SystemManagement.actorSystem
  
  implicit val timeout = defaultTimeout
  
  case class RouterInfo(router:ActorRef, requiresLogin:Boolean)
  
  case class HandlerInfo(constr:Constructor[AutowireApiImpl], allowedDuringHistory:Boolean)
  
  /**
   * Turn this config flag on to trace the path of API calls through some of the system.
   * 
   * WARNING: this produces *voluminous* output. It should never be turned on in production!
   */
  lazy val traceApi = Config.getBoolean("querki.test.traceApiCalls", false)
  
  /**
   * Map from API classes to the constructors for their handlers.
   */
  var sessionHandlers = Map.empty[String, HandlerInfo]
  var apiRouters = Map.empty[String, RouterInfo]
  
  // NOTE: we explicitly presume that the function is simply named under the API class. Is this always true?
  def apiName(req:autowire.Core.Request[String]) = req.path.dropRight(1).mkString(".")

  def registerApiImplFor[API, IMPL <: API with AutowireApiImpl](router:ActorRef, requiresLogin:Boolean, allowedDuringHistory:Boolean)(implicit apiTag:ClassTag[API], implTag:ClassTag[IMPL]) = {
    val api = apiTag.runtimeClass
    val apiName = api.getName
    val impl = implTag.runtimeClass
    // This asInstanceOf is sad, but for some reason it seems to be losing the constructed type otherwise.
    // (Some odd erasure behavior?)
    val constr = impl.getConstructor(classOf[AutowireParams], classOf[Ecology]).asInstanceOf[Constructor[AutowireApiImpl]]
    sessionHandlers += (apiName -> HandlerInfo(constr, allowedDuringHistory))
    apiRouters += (apiName -> RouterInfo(router, requiresLogin))
  }
  
  def requiresLogin(req:ClientRequest):Boolean = {
    val name = apiName(req.req)
    apiRouters.get(name).map(_.requiresLogin).getOrElse(throw new Exception(s"requiresLogin got unknown API $name"))
  }
  
  def routeRequest[R](req:ClientRequest)(cb: PartialFunction[Any, Future[R]]):Future[R] = {
    val name = apiName(req.req)
    apiRouters.get(name) match {
      case Some(router) => {
        apiTrace(s"  Sending call to ${req.req.path} to $router")
        ask(router.router, req)(timeout).flatMap {
          case msg:ClientResponse => cb(msg)
          case err:ClientError => cb(err)
          // This is essentially ClientResponse, but the contents are too large to send safely as a single
          // block. (Because of Akka message-size limits.) So we instead need to establish a channel to
          // stream it over to here:
          case OversizedResponse(streamer) => {
            val streamReceiver = actorSystem.actorOf(StringStream.receiverProps(streamer))
            ask(streamReceiver, StringStream.Start).flatMap {
              // We get this when the stream is done. Note that we explicitly assume that this message
              // from a *local* Actor is reasonably reliable, and that the message fits comfortably in
              // memory:
              case StringStream.ReceivedString(str) => {
                cb(ClientResponse(str))
              }
              
              case ex:AskTimeoutException => {
                cb(ClientError("Failed to get response!"))
              }
            }
          }
        }
      }
      case None => throw new Exception(s"handleSessionRequest got request for unknown API $name")
    }
  }
  
  def handleSessionRequest(req:autowire.Core.Request[String], params:AutowireParams, completeCb: Any => Unit = { dummy => }) = {
    sessionHandlers.get(apiName(req)) match {
      case Some(info) => {
        apiTrace(s"  Handling request for ${req.path.mkString(".")}(${req.args.values.mkString(", ")})")
        // Sanity-check that this call is allowed now.
        // TBD: this is kind of ugly here -- it's mixing semantic levels. In principle, this should be
        // refactored somewhere.
        if (History.isViewingHistory(params.rc) && !info.allowedDuringHistory)
          throw new Exception(s"Trying to call interface $info during History!")
        info.constr.newInstance(params, ecology).handleRequest(req, completeCb)
      }
      case None => throw new Exception(s"handleSessionRequest got request for unknown API ${apiName(req)}")
    }
  }
  
  def apiTrace(msg: => String):Unit = {
    if (traceApi)
      QLog.spew(msg)
  }
}