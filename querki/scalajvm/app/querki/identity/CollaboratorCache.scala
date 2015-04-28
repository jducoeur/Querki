package querki.identity

import scala.concurrent.duration._

import akka.actor._
import akka.event.LoggingReceive
import akka.pattern.AskTimeoutException

import querki.ecology._
import querki.session.UserSessionMessages
import querki.util.{Config, QLog, Requester}

class CollaboratorCache(val ecology:Ecology, val userId:UserId) extends Actor with Requester with EcologyMember  {
  lazy val IdentityAccess = interface[querki.identity.IdentityAccess]
  lazy val UserAccess = interface[querki.identity.UserAccess]
  
  // Flag telling us to clear our cache:
  object ClearCache
  
  lazy val cacheTimeout = Config.getDuration("querki.userSession.collaborators.timeout")
  
  var _allCollaborators:Option[Iterable[PublicIdentity]] = None
  // Are we in the middle of loading the Collaborators?
  var fetching = false
  // If we are in the middle of a load, build up a list of other outstanding requests:
  var currentRequests:Seq[(ActorRef, String)] = Seq.empty
  
  def sendResults(collabs:Iterable[PublicIdentity], termRaw:String, requester:ActorRef) = {
    val term = termRaw.toLowerCase()
    val results = collabs.filter(collab => collab.handle.toLowerCase().contains(term) || collab.name.toLowerCase().contains(term))
    requester ! UserSessionMessages.Collaborators(results)
  }

  def receive = LoggingReceive {
    case UserSessionMessages.GetCollaborators(_, identityId, term) => {
      _allCollaborators match {
        case Some(collabs) => {
          // We have all the necessary info, so just send the response:
          sendResults(collabs, term, sender)
        }
        case None => {
          if (fetching) {
            // There's already a request for the collaborators outstanding. Instead of duplicating effort,
            // simply log this request. When the fetch completes, this will be dispatched.
            currentRequests = currentRequests :+ (sender, term)
          } else {
            // Time to go fetch the full collaborator list:
            fetching = true
            // Get the IdentityIds of everyone I share a Space with:
	        val acquaintanceIds = UserAccess.getAcquaintanceIds(identityId)
	        // Get their Identities from the IdentityCache:
	        IdentityAccess.identityCache.request(IdentityCacheMessages.GetIdentities(acquaintanceIds)) foreach {
	          case IdentityCacheMessages.IdentitiesFound(identities) => {
	            import context.dispatcher
	            val collabs = identities.values
	            _allCollaborators = Some(collabs)
	            sendResults(collabs, term, sender)
	            // If other requests have come in the meantime, dispatch them as well:
	            currentRequests.map { request =>
	              val (requester, otherTerm) = request
	              sendResults(collabs, otherTerm, requester) 
	            }
	            currentRequests = Seq.empty
	            // We need to recheck this cache periodically, to make sure we stay decently up to date.
  	            // Ideally we would just tweak it as changes come in, but that's a pain; for now, just
	            // do a simple cache flush every now and then.
	            context.system.scheduler.scheduleOnce(cacheTimeout, self, ClearCache)
	            fetching = false
	          }
	          
	          case ex:AskTimeoutException => {
	            QLog.warn(s"CollaboratorCache timed out while trying to load collaborators for $userId")
	            sendResults(Iterable.empty, term, sender)
	            currentRequests.map { request =>
	              val (requester, otherTerm) = request
	              sendResults(Iterable.empty, otherTerm, requester) 
	            }
	            currentRequests = Seq.empty
	            fetching = false
	          }
	        }
          }
        }
      }
    }
    
    case ClearCache => _allCollaborators = None
  }
}

object CollaboratorCache {
  // TODO: the following Props signature is now deprecated, and should be replaced (in Akka 2.2)
  // with "Props(classOf(Space), ...)". See:
  //   http://doc.akka.io/docs/akka/2.2.3/scala/actors.html
  def actorProps(ecology:Ecology, id:UserId):Props = Props(new CollaboratorCache(ecology, id)) 
}
