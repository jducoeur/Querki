package querki.conversations

import akka.actor._
import akka.persistence._

import querki.conversations.messages.Comment
import querki.globals._
import querki.identity.{IdentityId, User}
import querki.persistence.PersistentQuerkiActor
import querki.types.ModelTypeDefiner
import querki.util.TimeoutChild

/**
 * This represents the active Conversations attached to a particular Thing. Note that essentially
 * all functionality comes from its base traits.
 */
class ThingConversationsActor(initState:SpaceState, thingId:OID, notifier:ActorRef, e:Ecology) 
  extends ThingConversationsCore(initState, thingId)(e) with PersistentQuerkiActor with TimeoutChild with ModelTypeDefiner 
{
  val timeoutConfig = "querki.conversations.timeout"
  
  def notifyNewComment(req:User, comment:Comment, parentAuthors:Seq[IdentityId]) = {
    notifier ! SpaceConversationNotifier.NotifyNewComment(req, comment, parentAuthors)
  }
}

object ThingConversationsActor {
  def actorProps(initState:SpaceState, thingId:OID, notifier:ActorRef, e:Ecology) = Props(classOf[ThingConversationsActor], initState, thingId, notifier, e)
}
