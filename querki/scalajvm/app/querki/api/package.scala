package querki

import scala.concurrent.Future
import scala.reflect.ClassTag

import akka.actor.ActorRef

import models.{Thing}

import querki.globals._

import querki.data._
import querki.identity.PublicIdentity
import querki.pages.PageDetails
import querki.values.RequestContext

package object api {
  trait ClientApi extends EcologyInterface {
    def requestInfo(rc:RequestContext)(implicit state:SpaceState):RequestInfo
    
    def thingInfo(t:Thing, rc:RequestContext)(implicit state:SpaceState):ThingInfo
    
    def identityInfo(identity:PublicIdentity):IdentityInfo
    
    def propInfo(prop:AnyProp, rc:RequestContext)(implicit state:SpaceState):PropInfo
    
    def propValInfo(t:Thing, rc:RequestContext)(implicit state:SpaceState):Seq[PropValInfo]
    
    def handleCommonFunction(rc:RequestContext, req:autowire.Core.Request[String]):Future[ClientAnswer]
  }
  
  /**
   * Post-init-time interface for registering handlers for APIs.
   */
  trait ApiRegistry extends EcologyInterface {
    /**
     * Registers the given IMPL as the implementation for the specified API. Should be called during postInit()!
     * 
     * @tparam API The API class, usually declared in shared
     * @tparam IMPL The implementation of that API
     * @param router The Actor that will receive these calls, wrapped in a ClientRequest. This is may be either
     *   a direct handler or, more often, a router (usually a ClusterRegion) that will send it on to its
     *   actual handler based on the RC.
     * @param requiresLogin Iff true, and the Client isn't logged in, this request will bounce at the
     *   controller level, never even being invoked. This is a belt-and-suspenders check for security and
     *   sanity. It says nothing about *who* the invoker is, merely that this API makes no sense if you
     *   aren't logged in. This defaults to true; you must set it to false iff this API is legal *without* login.
     */
    def registerUserSessionImplFor[API, IMPL <: API with AutowireApiImpl](router:ActorRef, requiresLogin:Boolean = true)(implicit apiTag:ClassTag[API], implTag:ClassTag[IMPL])
  }
  
  /**
   * Allows Actors to invoke Autowire Requests that come to them.
   */
  trait ApiInvocation extends EcologyInterface {
    /**
     * This is mainly for the edge of the system, to check whether a given request is legal for Anonymous use.
     */
    def requiresLogin(req:ClientRequest):Boolean
    /**
     * Sends this request to the registered router for this request's API. When the request is fully
     * processed, it will call cb with the results, and the result of *that* will be returned.
     */
    def routeRequest[R](req:ClientRequest)(cb: PartialFunction[Any, Future[R]]):Future[R]
    /**
     * This should be called by an Actor that ultimately receives ClientRequests. The Actor should
     * build an appropriate AutowireParams for this environment, then call this to invoke it. Note
     * that the Actor *must* provide a Requester for the handler's use -- typically this Actor should
     * simply be a Requester, but it can in principle be delegated.
     */
    def handleSessionRequest(req:autowire.Core.Request[String], params:AutowireParams):Unit
  }
}
