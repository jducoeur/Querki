package querki.identity

import akka.actor._

import models.{OID}

import querki.ecology._
import querki.session.UserSessionMessages.UserSessionMsg

/**
 * The front-end cache for Users and Identities. All interactions with these tables should
 * go through here, or at least inform this Cache about invalidating operations.
 * 
 * TODO: this is currently calling UserPersistence directly. In principle, it shouldn't be
 * doing do, since those are blocking operations. Instead, those calls should be doled out
 * to workers, so that they don't block other requests for the cache.
 */
private[identity] class IdentityCache(val ecology:Ecology) extends Actor with EcologyMember {
  
  import IdentityCacheMessages._
  
  lazy val UserAccess = interface[UserAccess]
  lazy val UserSessions = interface[querki.session.Session]
  
  lazy val sessionManager = UserSessions.sessionManager
  
  var identities = Map.empty[OID, FullIdentity]
  
  def fetch(id:OID):Option[FullIdentity] = {
    identities.get(id) match {
      case Some(identity) => Some(identity)
      case None => {
        UserAccess.getFullIdentity(id) match {
          case Some(identity) => {
            identities += (id -> identity)
            Some(identity)
          }
          case None => None
        }
      }
    }    
  }
  
  def receive = {
    case GetIdentityRequest(id) => {
      fetch(id) match {
        case Some(identity) => sender ! IdentityFound(identity)
        case None => sender ! IdentityNotFound
      }
    }
    
    case GetIdentities(ids) => {
      val result = (Map.empty[OID, PublicIdentity] /: ids.toSet) { (curmap, id) =>
        fetch (id) match {
          case Some(identity) => curmap + (id -> identity)
          case None => curmap
        }
      }
      sender ! IdentitiesFound(result)
    }
    
    case InvalidateCacheForIdentity(id) => {
      identities = identities - id
    }
    
    case RouteToUsers(identityIds, msg) => {
      identityIds.foreach { id =>
        fetch(id) match {
          case Some(identity) => sessionManager.forward(msg.copyTo(identity.userId))
          case None => {}
        }
      }
    }
  }
}

object IdentityCacheMessages {
  case class GetIdentityRequest(id:OID)
  case class IdentityFound(identity:PublicIdentity)
  case object IdentityNotFound
  
  case class GetIdentities(ids:Seq[OID])
  case class IdentitiesFound(identities:Map[OID,PublicIdentity])
  
  case class InvalidateCacheForIdentity(id:OID)

  /**
   * Sends the given msg to all of the UserSessions behind the given identityIds.
   */
  case class RouteToUsers(identityIds:Seq[OID], msg:UserSessionMsg)
}
