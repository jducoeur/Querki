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
     */
    def registerUserSessionImplFor[API, IMPL <: API with AutowireApiImpl](router:ActorRef)(implicit apiTag:ClassTag[API], implTag:ClassTag[IMPL])
  }
  
  /**
   * Allows Actors to invoke Autowire Requests that come to them.
   */
  trait ApiInvocation extends EcologyInterface {
    def routeRequest[R](req:ClientRequest)(cb: PartialFunction[Any, Future[R]]):Future[R]
    def handleSessionRequest(req:autowire.Core.Request[String], params:AutowireParams):Unit
  }
}
