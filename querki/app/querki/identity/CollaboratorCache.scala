package querki.identity

import scala.concurrent.duration._

import akka.actor._
import akka.event.LoggingReceive

import querki.ecology._
import querki.session.UserSessionMessages
import querki.util.{QLog, Requester}

class CollaboratorCache(val ecology:Ecology, val userId:UserId) extends Actor with Requester with EcologyMember  {
  lazy val IdentityAccess = interface[querki.identity.IdentityAccess]
  lazy val UserAccess = interface[querki.identity.UserAccess]
  
  var _allCollaborators:Option[Iterable[PublicIdentity]] = None
  
  def sendResults(collabs:Iterable[PublicIdentity], term:String) = {
    val results = collabs.filter(collab => collab.handle.contains(term) || collab.name.contains(term))
    sender ! UserSessionMessages.Collaborators(results)
  }

  def receive = LoggingReceive {
    case UserSessionMessages.GetCollaborators(_, identityId, term) => {
      _allCollaborators match {
        case Some(collabs) => {
          sendResults(collabs, term)
        }
        case None => {
          // TODO: detect if there is an outstanding fetch, and wait for that if so:
	      val acquaintanceIds = UserAccess.getAcquaintanceIds(identityId)
	      IdentityAccess.identityCache.request(IdentityCacheMessages.GetIdentities(acquaintanceIds)) {
	        case IdentityCacheMessages.IdentitiesFound(identities) => {
	          _allCollaborators = Some(identities.values)
	          sendResults(identities.values, term)
	          // TODO: time out the collaborator cache. Fetch the timeout delay from config. Enhance Config to
	          // have a proper mechanism for defining durations. See TimeoutChild for ideas.
//	          context.system.scheduler.scheduleOnce(delay)(f)
	        }
	      }
        }
      }
    }
  }
}

object CollaboratorCache {
  // TODO: the following Props signature is now deprecated, and should be replaced (in Akka 2.2)
  // with "Props(classOf(Space), ...)". See:
  //   http://doc.akka.io/docs/akka/2.2.3/scala/actors.html
  def actorProps(ecology:Ecology, id:UserId):Props = Props(new CollaboratorCache(ecology, id)) 
}
