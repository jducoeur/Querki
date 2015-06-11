package querki.session

import akka.actor._
import akka.event.LoggingReceive

import models.OID

import querki.ecology._
import querki.identity.User
import querki.session.messages._
import querki.spaces.messages.{SessionRequest, CurrentState, UserValuePersistRequest}
import querki.util._
import querki.values.SpaceState

private [session] class UserSpaceSessions(val spaceId:OID, val spaceRouter:ActorRef)
  extends Actor with EcologyMember with RoutingParent[User]
{
  lazy val SpacePersistenceFactory = interface[querki.spaces.SpacePersistenceFactory]
  
  val persister = SpacePersistenceFactory.getUserValuePersister(spaceId)
  
  var state:Option[SpaceState] = None
  
  def createChild(key:User):ActorRef = {
    // Sessions need a special dispatcher so they can use Stash. (Seriously? Unfortunate leakage in the Akka API.)
    context.actorOf(UserSpaceSession.actorProps(spaceId, key, spaceRouter, persister).withDispatcher("session-dispatcher"), key.id.toString)
  }
  
  override def initChild(child:ActorRef) = state.map(child ! CurrentState(_))
  
  def receive = LoggingReceive {
    /**
     * The Space has sent an updated State, so tell everyone about it.
     */
    case msg @ CurrentState(s) => {
      state = Some(s)
      children.foreach(session => session.forward(msg))
    }
    
    case GetActiveSessions => sender ! ActiveSessions(children.size)
    
    // HACK: messages heading for the User Value Persister:
    case msg:UserValuePersistRequest => persister.forward(msg)

    /**
     * Message to forward to a UserSpaceSession. Create the session, if needed.
     */
    case msg @ SessionRequest(requester, _, payload) => routeToChild(requester, msg)
  }

}

object UserSpaceSessions {
  // TODO: the following Props signature is now deprecated, and should be replaced (in Akka 2.2)
  // with "Props(classOf(Space), ...)". See:
  //   http://doc.akka.io/docs/akka/2.2.3/scala/actors.html
  def actorProps(spaceId:OID, spaceRouter:ActorRef):Props = Props(new UserSpaceSessions(spaceId, spaceRouter))
}
