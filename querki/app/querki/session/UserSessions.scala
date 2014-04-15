package querki.session

import akka.actor._
import akka.event.LoggingReceive

import models.OID

import querki.ecology._
import querki.session.messages._
import querki.spaces.messages.{SessionRequest, CurrentState}
import querki.values.SpaceState

private [session] class UserSessions(val ecology:Ecology, val spaceId:OID)
  extends Actor with EcologyMember
{
  var state:Option[SpaceState] = None
  
  var sessions = Map.empty[OID, ActorRef]
  
  def receive = LoggingReceive {
    /**
     * The Space has sent an updated State, so tell everyone about it.
     */
    case msg @ CurrentState(s) => {
      state = Some(s)
      sessions.values.foreach(session => session.forward(msg))
    }
    
    case GetActiveSessions => sender ! ActiveSessions(sessions.size)

    /**
     * Message to forward to a UserSession. Create the session, if needed.
     */
    case SessionRequest(requester, _, _, payload) => {
      val reqId = requester.id
      val session = sessions.get(reqId) match {
        case Some(s) => s
        case None => {
          // Sessions need a special dispatcher so they can use Stash. (Seriously? Unfortunate leakage in the Akka API.)
          val s = context.actorOf(UserSession.actorProps(ecology, spaceId).withDispatcher("session-dispatcher"), reqId.toString)
          sessions = sessions + (reqId -> s)
          // If possible, send the current state along, to bootstrap the session:
          state.map(s ! CurrentState(_))
          s
        }
      }
      session.forward(payload)
    }
  }

}

object UserSessions {
  // TODO: the following Props signature is now deprecated, and should be replaced (in Akka 2.2)
  // with "Props(classOf(Space), ...)". See:
  //   http://doc.akka.io/docs/akka/2.2.3/scala/actors.html
  def actorProps(ecology:Ecology, spaceId:OID):Props = Props(new UserSessions(ecology, spaceId))
}
