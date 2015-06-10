package querki.spaces

import akka.actor._
import akka.event.LoggingReceive

import org.querki.requester._

import models.OID

import querki.conversations.messages.{ActiveThings, GetActiveThings}
import querki.ecology._
import querki.photos.PhotoUploadActor
import querki.session.UserSpaceSessions
import querki.session.messages._
import querki.spaces.messages._
import querki.util.ClusterTimeoutChild
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
private[spaces] class SpaceRouter(val ecology:Ecology) 
  extends Actor with EcologyMember with Requester with ClusterTimeoutChild
{  
  lazy val Conversations = interface[querki.conversations.Conversations]
  lazy val persistenceFactory = interface[SpacePersistenceFactory]
  
  lazy val spaceId:OID = OID(self.path.name)
  
  // How long we can be inactive before timing out this entire hive:
  def timeoutConfig:String = "querki.space.timeout"
  
  // The components of the troupe:
  var conversations:ActorRef = null
  var space:ActorRef = null
  var sessions:ActorRef = null
  var members:ActorRef = null
  
  var state:SpaceState = null

  override def preStart() = {
    space = context.actorOf(Space.actorProps(ecology, persistenceFactory, self, spaceId), "Space")
    sessions = context.actorOf(UserSpaceSessions.actorProps(ecology, spaceId, self), "Sessions")
    conversations = context.actorOf(Conversations.conversationActorProps(persistenceFactory, spaceId, self), "Conversations") 
    members = context.actorOf(SpaceMembersActor.actorProps(ecology, spaceId, self), "Members")
    super.preStart()
  }
  
  def receive = LoggingReceive (handleRequestResponse orElse {
    
    /**
     * The Space has sent an updated State, so tell everyone about it.
     */
    case msg @ CurrentState(curState) => {
      state = curState
      conversations.forward(msg)
      sessions.forward(msg)
      members.forward(msg)
    }
    
    /**
     * Admin has asked all of the Spaces to give a quick status report.
     */
    case GetSpacesStatus(requester) => {
      for {
        ActiveThings(nConvs) <- conversations.request(GetActiveThings)
        ActiveSessions(nSessions) <- sessions.request(GetActiveSessions)
      }
        sender ! SpaceStatus(spaceId, state.displayName, nConvs, nSessions)
    }
    
    // Message for the Conversation system:
    case req:ConversationRequest => conversations.forward(req)
    
    // Message for a Session:
    case req:SessionRequest => sessions.forward(req)
    
    // HACK: messages heading for the User Value Persister:
    case msg:UserValuePersistRequest => sessions.forward(msg)
    
    // Messages for the SpaceMembersActor:
    case msg:SpaceMembersMessage => members.forward(msg)
    
    // Request for an upload actor under this Space. We create it as part of the troupe, but it's
    // basically anonymous after creation:
    case msg:BeginProcessingPhoto => {
      val worker = context.actorOf(PhotoUploadActor.actorProps(ecology))
      worker.forward(msg)
      sender ! worker
    }
    
    // Message for the Space:
    case msg:CreateSpace => space.forward(msg)
    // FALLBACK -- NOTHING SHOULD GO BELOW HERE:
    case msg:SpaceMessage => space.forward(msg)
  })
}

object SpaceRouter {
  def actorProps(ecology:Ecology):Props = Props(classOf[SpaceRouter], ecology)
}
