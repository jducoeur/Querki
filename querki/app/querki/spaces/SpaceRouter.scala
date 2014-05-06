package querki.spaces

import akka.actor._
import akka.event.LoggingReceive

import models.OID

import querki.conversations.messages.{ActiveThings, GetActiveThings}
import querki.ecology._
import querki.session.UserSessions
import querki.session.messages._
import querki.spaces.messages._
import querki.util.Requester
import querki.values.SpaceState

/**
 * The SpaceRouter is the root of the "troupe" of Actors for a single Space. It owns all of the Actors that
 * are involved with this specific Space, routes messages to them, and deals with the Space's lifecycle.
 * It is factored out separately so that the Space doesn't have the overhead of all that routing, and
 * for separations of concerns in general. The only downside is that this *does* need to know about all
 * the message types coming to the Space, and where each one goes.
 * 
 * IMPORTANT: the entire troupe for a single Space should live on the same Node as the SpaceRouter. This
 * is mainly for efficiency -- the hive passes the SpaceState around frequently, and we do *not* want to
 * be serializing that.
 */
private[spaces] class SpaceRouter(val ecology:Ecology, persistenceFactory:SpacePersistenceFactory, val spaceId:OID) 
  extends Actor with EcologyMember with Requester
{
  
  lazy val Conversations = interface[querki.conversations.Conversations]
  
  // The components of the troupe:
  var conversations:ActorRef = null
  var space:ActorRef = null
  var sessions:ActorRef = null
  
  var state:SpaceState = null

  override def preStart() = {
    space = context.actorOf(Space.actorProps(ecology, persistenceFactory, self, spaceId), "Space")
    sessions = context.actorOf(UserSessions.actorProps(ecology, spaceId, self), "Sessions")
    conversations = context.actorOf(Conversations.conversationActorProps(persistenceFactory, spaceId, self), "Conversations") 
  }
  
  def receive = LoggingReceive {
    
    /**
     * The Space has sent an updated State, so tell everyone about it.
     */
    case msg @ CurrentState(curState) => {
      state = curState
      conversations.forward(msg)
      sessions.forward(msg)
    }
    
    /**
     * Admin has asked all of the Spaces to give a quick status report.
     */
    case GetSpacesStatus(requester) => {
      conversations.request(GetActiveThings) {
        case ActiveThings(nConvs) => {
          sessions.request(GetActiveSessions) {
            case ActiveSessions(nSessions) => {
              sender ! SpaceStatus(spaceId, state.displayName, nConvs, nSessions)
            }
          }
        }
      }
    }
    
    // Message for the Conversation system:
    case req:ConversationRequest => conversations.forward(req)
    
    // Message for a Session:
    case req:SessionRequest => sessions.forward(req)
    
    // HACK: messages heading for the User Value Persister:
    case msg:UserValuePersistRequest => sessions.forward(msg)
    
    // Message for the Space:
    case msg:CreateSpace => space.forward(msg)
    case msg:SpaceMessage => space.forward(msg)
    case msg:SpacePluginMsg => space.forward(msg)
  }
}

object SpaceRouter {
  // TODO: the following Props signature is now deprecated, and should be replaced (in Akka 2.2)
  // with "Props(classOf(Space), ...)". See:
  //   http://doc.akka.io/docs/akka/2.2.3/scala/actors.html
  def actorProps(ecology:Ecology, persistenceFactory:SpacePersistenceFactory, spaceId:OID):Props = Props(new SpaceRouter(ecology, persistenceFactory, spaceId))
}
