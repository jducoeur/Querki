package querki.api

import java.lang.reflect.Constructor

import scala.concurrent.duration._
import scala.concurrent.Future
import scala.reflect.ClassTag

import akka.actor.ActorRef
import akka.util.Timeout

import querki.ecology._
import querki.globals._
import Implicits.execContext

/**
 * @author jducoeur
 */
class ApiManagement(e:Ecology) extends QuerkiEcot(e) with ApiRegistry with ApiInvocation {
  implicit val timeout = Timeout(5 seconds)
  
  /**
   * Map from API classes to the constructors for their handlers.
   */
  var sessionHandlers = Map.empty[String, Constructor[AutowireApiImpl]]
  var apiRouters = Map.empty[String, ActorRef]
  
  // NOTE: we explicitly presume that the function is simply named under the API class. Is this always true?
  def apiName(req:autowire.Core.Request[String]) = req.path.dropRight(1).mkString(".")

  def registerUserSessionImplFor[API, IMPL <: API with AutowireApiImpl](router:ActorRef)(implicit apiTag:ClassTag[API], implTag:ClassTag[IMPL]) = {
    val api = apiTag.runtimeClass
    val apiName = api.getName
    val impl = implTag.runtimeClass
    // This asInstanceOf is sad, but for some reason it seems to be losing the constructed type otherwise.
    // (Some odd erasure behavior?)
    val constr = impl.getConstructor(classOf[AutowireParams], classOf[Ecology]).asInstanceOf[Constructor[AutowireApiImpl]]
    sessionHandlers += (apiName -> constr)
    apiRouters += (apiName -> router)
  }
  
  def routeRequest[R](req:ClientRequest)(cb: PartialFunction[Any, Future[R]]):Future[R] = {
    val name = apiName(req.req)
    apiRouters.get(name) match {
      case Some(router) => akka.pattern.ask(router, req)(timeout).flatMap(cb)
      case None => throw new Exception(s"handleSessionRequest got request for unknown API $name")
    }
  }
  
  def handleSessionRequest(req:autowire.Core.Request[String], params:AutowireParams) = {
    sessionHandlers.get(apiName(req)) match {
      case Some(constr) => constr.newInstance(params, ecology).handleRequest(req)
      case None => throw new Exception(s"handleSessionRequest got request for unknown API ${apiName(req)}")
    }
  }  
}