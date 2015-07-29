package querki.identity

import scala.concurrent.Future

import akka.actor._

import org.querki.requester.Requester

import models.{OID}

import querki.ecology._
import querki.globals._
import querki.session.UserSessionMessages.UserSessionMsg

/**
 * The front-end cache for Users and Identities. All interactions with these tables should
 * go through here, or at least inform this Cache about invalidating operations.
 * 
 * TODO: on principle, we should time out entries in this cache every now and then, maybe once an
 * hour, so that if something caches we pick it up eventually. This is low-priority for the moment,
 * since there isn't much you can *do* to an Identity.
 */
private[identity] class IdentityCache(val ecology:Ecology) extends Actor with Requester with EcologyMember {
  
  import IdentityCacheMessages._
  
  lazy val UserSessions = interface[querki.session.Session]
  
  lazy val sessionManager = UserSessions.sessionManager
  
  var identities = Map.empty[OID, FullIdentity]
  
  // The worker that deals with actual DB lookups, so that those don't block the rest of the cache.
  // TODO: I suspect this should be a pool of shared workers rather than a single one per IdentityCache,
  // but this is good enough for now.
  lazy val worker = context.actorOf(Props(classOf[IdentityCacheFetcher], ecology))
  
  def fetchAndThen(id:OID)(cb:IdentityResponse => Unit) = {
    identities.get(id) match {
      case Some(identity) => cb(IdentityFound(identity))
      case None => worker.requestFor[IdentityResponse](GetIdentityRequest(id)) foreach {
        case resp @ IdentityFound(identity) => {
          identities += (id -> identity)
          cb(resp)
        }
        case IdentityNotFound => cb(IdentityNotFound)
      }
    }
  }
  
  def receive = {
    case GetIdentityRequest(id) => {
      fetchAndThen(id) { resp => sender ! resp }
    }
    
    case InvalidateCacheForIdentity(id) => {
      identities = identities - id
    }
    
    case RouteToUser(id, router, msg) => {
      fetchAndThen(id) {
        case IdentityFound(identity) => router.forward(msg.toUser(identity.userId))
        case IdentityNotFound => {}
      }
    }
  }
}

object IdentityCache {
  def actorProps(ecology:Ecology) = Props(classOf[IdentityCache], ecology)
}

/**
 * A small internal Actor that does the actual database lookups for IdentityCache. This is split out
 * so that IdentityCache can stay fast and non-blocking for cache hits, and only slow down for the misses.
 */
private [identity] class IdentityCacheFetcher(val ecology:Ecology) extends Actor with EcologyMember {
  import IdentityCacheMessages._
  
  lazy val UserAccess = interface[UserAccess]
  
  def receive = {
    case GetIdentityRequest(id) => {
      UserAccess.getFullIdentity(id) match {
        case Some(identity) => sender ! IdentityFound(identity)
        case None => sender ! IdentityNotFound
      }      
    }
  }
}

object IdentityCacheMessages {
  sealed trait IdentityRequest {
    def id:OID
  }
  
  case class GetIdentityRequest(id:OID) extends IdentityRequest
  sealed trait IdentityResponse
  case class IdentityFound(identity:FullIdentity) extends IdentityResponse
  case object IdentityNotFound extends IdentityResponse
  
  case class InvalidateCacheForIdentity(id:OID) extends IdentityRequest
  
  /**
   * Sends the given msg to the UserSession behind the given identityIds.
   */
  case class RouteToUser(id:OID, router:ActorRef, msg:UserRouteableMessage) extends IdentityRequest
}
