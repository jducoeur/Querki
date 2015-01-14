package querki.spaces

import scala.concurrent.Future

import akka.actor._
import akka.event.LoggingReceive
import akka.pattern.pipe

import querki.globals._
import Implicits.execContext

import querki.spaces.messages._
import querki.util.Requester

private [spaces] class SpaceMembersActor(e:Ecology, val spaceId:OID, val spaceRouter:ActorRef)
  extends Actor with Stash with Requester with EcologyMember
{
  implicit val ecology = e
  
  lazy val Person = interface[querki.identity.Person]

  def receive = {
    case CurrentState(state) => {
      unstashAll()
      context.become(normalReceive(state))
    }
    
    case _ => stash()
  }
  
  // Normal behavior -- at any given time, state is the current known SpaceState
  def normalReceive(state:SpaceState):Receive = {
    case CurrentState(newState) => context.become(normalReceive(newState))
    
    // Someone is attempting to join this Space:
    case JoinRequest(_, _, _, rcRaw) => {
      val rc = rcRaw + state
      val result:Option[Future[JoinResult]] = Person.acceptInvitation(rc) {
        case ThingFound(id, state) => Future.successful(Joined) 
        case ThingError(error, stateOpt) => Future.successful(JoinFailed(error))
        case _ => Future.successful(JoinFailed(new PublicException("Space.join.unknownError", state.displayName)))
      }
      result match {
        case Some(fut) => pipe(fut) to sender
        case None => sender ! JoinFailed(new PublicException("Space.join.unknownError", state.displayName))
      }
    }
  }
}

private [spaces] object SpaceMembersActor {
  def actorProps(e:Ecology, spaceId:OID, spaceRouter:ActorRef) = Props(classOf[SpaceMembersActor], e, spaceId, spaceRouter)  
}
