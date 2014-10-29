package querki.conversations

import scala.concurrent.{Future, Promise}

import akka.actor._

import querki.globals._

import querki.identity.IdentityCacheMessages.{GetIdentities, IdentitiesFound}
import querki.session.SessionApiImpl
import querki.spaces.messages.{ConversationRequest, ThingError}
import querki.util.Requester

import messages._

/**
 * Implements the Client API for Conversations.
 * 
 * Note that this trait is specifically design to be mixed into the UserSession.
 */
trait ConversationFunctionsImpl extends ConversationFunctions with SessionApiImpl { self:Actor with Requester =>
  
  lazy val IdentityAccess = interface[querki.identity.IdentityAccess]
  
  def getIds(node:ConversationNode):Set[OID] = {
    getIds(node.responses) + node.comment.authorId
  }
  
  def getIds(nodes:Seq[ConversationNode]):Set[OID] = {
    (Set.empty[OID] /: nodes) { (set, node) => set ++ getIds(node) }
  }
  
  def getConversationsFor(thingId:String):Future[ConversationInfo] = withThing(thingId) { thing =>
    // TODO: this pattern, which melds a Requester with Futures, seems useful. Can we abstract it a bit?
    val promise = Promise[ConversationInfo]
    spaceRouter.request(ConversationRequest(rc.requesterOrAnon, rc.ownerId, rc.state.get.id, GetConversations(rc.thing.get.id))) {
      case ThingConversations(convs) => {
        // TODO: this is a beautiful example of where what we really *want* is to make Requester monadic, at least
        // conceptually: these nested requests feel like they belong in a for comprehension. Is that possible? It's
        // certainly worth thinking about, anyway.
        IdentityAccess.identityCache.request(GetIdentities(getIds(convs).toSeq)) {
	      case IdentitiesFound(identities) => {
	    	promise.success(ConversationInfo(false, false, Seq.empty))
	      }
	      case _ => promise.failure(new Exception("Unable to find Conversation identities!"))
        }
      }
      case ThingError(ex, stateOpt) => promise.failure(ex)
    }
    promise.future
  }
}
