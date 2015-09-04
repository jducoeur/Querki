package querki.api

import java.lang.reflect.Constructor

import scala.concurrent.duration._
import scala.reflect.ClassTag

import akka.actor.ActorRef
import akka.util.Timeout

import querki.ecology._
import querki.globals._

/**
 * @author jducoeur
 */
class ApiManagement(e:Ecology) extends QuerkiEcot(e) with ApiRegistry with ApiInvocation {
  implicit val timeout = defaultTimeout
  
  case class RouterInfo(router:ActorRef, requiresLogin:Boolean)
  
  /**
   * Map from API classes to the constructors for their handlers.
   */
  var sessionHandlers = Map.empty[String, Constructor[AutowireApiImpl]]
  var apiRouters = Map.empty[String, RouterInfo]
  
  // NOTE: we explicitly presume that the function is simply named under the API class. Is this always true?
  def apiName(req:autowire.Core.Request[String]) = req.path.dropRight(1).mkString(".")

  def registerApiImplFor[API, IMPL <: API with AutowireApiImpl](router:ActorRef, requiresLogin:Boolean)(implicit apiTag:ClassTag[API], implTag:ClassTag[IMPL]) = {
    val api = apiTag.runtimeClass
    val apiName = api.getName
    val impl = implTag.runtimeClass
    // This asInstanceOf is sad, but for some reason it seems to be losing the constructed type otherwise.
    // (Some odd erasure behavior?)
    val constr = impl.getConstructor(classOf[AutowireParams], classOf[Ecology]).asInstanceOf[Constructor[AutowireApiImpl]]
    sessionHandlers += (apiName -> constr)
    apiRouters += (apiName -> RouterInfo(router, requiresLogin))
  }
  
  def requiresLogin(req:ClientRequest):Boolean = {
    val name = apiName(req.req)
    apiRouters.get(name).map(_.requiresLogin).getOrElse(throw new Exception("requiresLogin got unknown API $name"))
  }
  
  def routeRequest[R](req:ClientRequest)(cb: PartialFunction[Any, Future[R]]):Future[R] = {
    val name = apiName(req.req)
    apiRouters.get(name) match {
      case Some(router) => akka.pattern.ask(router.router, req)(timeout).flatMap(cb)
      case None => throw new Exception(s"handleSessionRequest got request for unknown API $name")
    }
  }
  
  def handleSessionRequest(req:autowire.Core.Request[String], params:AutowireParams, completeCb: Any => Unit = { dummy => }) = {
    sessionHandlers.get(apiName(req)) match {
      case Some(constr) => constr.newInstance(params, ecology).handleRequest(req, completeCb)
      case None => throw new Exception(s"handleSessionRequest got request for unknown API ${apiName(req)}")
    }
  }  
}