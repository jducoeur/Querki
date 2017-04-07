package querki.conversations

import akka.actor._
import akka.contrib.pattern.ReceivePipeline

import querki.conversations.messages._
import querki.globals._
import querki.spaces.messages._
import querki.util._

/**
 * This Actor is part of a Space's Troupe; it manages the sub-Troupe of active
 * ThingConversationsActors underneath it. Basically, it's the central manager
 * for conversations in this Space, but it doesn't *do* much aside from routing.
 * 
 * This is a QuerkiBootableActor, and does not become fully active until it receives
 * an initial SpaceState.
 * 
 * Note that this is a RoutingParent, which deals with most of the routing and timeout
 * infrastructure.
 */
private [conversations] class SpaceConversationsManager(e:Ecology, router:ActorRef) 
  extends QuerkiBootableActor(e) with ReceivePipeline with RoutingParent[OID]
{ 
  var _state:Option[SpaceState] = None
  def state = _state.get
  
  lazy val notifier = context.actorOf(SpaceConversationNotifier.actorProps(e, state, router))
  
  // From RoutingParent
  def createChild(thingId:OID):ActorRef = context.actorOf(ThingConversationsActor.actorProps(state, thingId, notifier, ecology))
  
  def bootReceive:Receive = {
    case CurrentState(current) => {
      _state = Some(current)
      doneBooting()
    }
  }
  
  def doReceive:Receive = {
    case msg @ CurrentState(current) => {
      _state = Some(current)
      routeToAll(msg)
      notifier ! msg
    }
    
    case GetActiveThings => sender ! ActiveThings(nChildren)
    
    case msg @ SpaceSubsystemRequest(req, _, payload) => {
      val thingId = payload match {
        case GetConversations(tid) => tid
        case NewComment(comment) => comment.thingId
        case DeleteComment(tid, _) => tid
      }
      
      routeToChild(thingId, msg)
    }
  }
}
