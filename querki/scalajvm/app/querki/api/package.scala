package querki

import scala.concurrent.Future
import scala.reflect.ClassTag

import akka.actor.ActorRef

import models.{Thing}

import querki.globals._

import querki.data._
import querki.identity.{PublicIdentity, User}
import querki.values.RequestContext

package object api {
  
  /**
   * If a Thing is part of the common API -- if it is defined in StandardThings -- then we have
   * to be careful about name duplication. Instead of defining the name as a literal String,
   * use this to fetch the name that the API expects. (In other words, the API is the
   * authoritative source, and we're avoiding duplicating those strings.)
   */
  def commonName(f:StandardThings => ThingInfo):String = {
    f(Names).linkName.get
  }
  
  trait ClientApi extends EcologyInterface {
    def thing2TID(t:Thing):TID
    def OID2TID(oid:OID):TID
    
    def requestInfo(rc:RequestContext)(implicit state:SpaceState):Future[RequestInfo]
    def rootRequestInfo(rc:RequestContext):Future[RequestInfo]
    
    def thingInfo(t:Thing, rc:RequestContext)(implicit state:SpaceState):Future[ThingInfo]
    
    def spaceInfo(info:querki.spaces.messages.SpaceInfo):SpaceInfo
    
    def spaceInfo(state:SpaceState, user:User):SpaceInfo
    
    def identityInfo(identity:PublicIdentity):IdentityInfo
    
    def userInfo(uopt:Option[User]):Future[Option[UserInfo]]
    
    def propInfo(prop:AnyProp, rc:RequestContext)(implicit state:SpaceState):PropInfo
    
    def propValInfo(t:Thing, rc:RequestContext)(implicit state:SpaceState):Future[Seq[PropValInfo]]
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
    def registerApiImplFor[API, IMPL <: API with AutowireApiImpl](router:ActorRef, requiresLogin:Boolean = true, allowedDuringHistory:Boolean = false)(implicit apiTag:ClassTag[API], implTag:ClassTag[IMPL])
  }
  
  /**
   * Allows Actors to invoke Autowire Requests that come to them.
   */
  trait ApiInvocation extends EcologyInterface {
    /**
     * Spews the given message if API tracing is turned on. Does not evaluate msg otherwise. (Yay, call-by-name!)
     */
    def apiTrace(msg: => String):Unit
    
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
     * 
     * Iff completeCb is specified, it will be called after the call is complete, with the success result or
     * the thrown Exception. IMPORTANT: this is called in an
     * arbitrary Future, and you should not count on it being synchronous with the calling Actor!
     */
    def handleSessionRequest(req:autowire.Core.Request[String], params:AutowireParams, completeCb: Any => Unit = { dummy => }):Unit
  }
}
