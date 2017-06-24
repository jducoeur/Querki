package querki.publication

import akka.actor._
import akka.persistence._

import funcakka._
import org.querki.requester._

import querki.globals._
import querki.persistence.UseKryo
import querki.spaces.SpaceMessagePersistence.SpaceEvent

/**
 * Message published to the troupe when there is new information in the Publication State. It
 * is the responsibility of the UserSpaceSessions to incorporate this.
 */
case class CurrentPublicationState(state:SpaceState)

trait PublicationStateMessage

/**
 * Message that says to add the given Events to the Publication History. Responds with a
 * CurrentPublicationState.
 */
case class AddPublicationEvents(evts:List[SpaceEvent with UseKryo]) extends PublicationStateMessage

class InPublicationStateActor(val ecology:Ecology, val id:OID, val router:ActorRef)
  extends InPublicationStateCore with RealActorCore with PersistentActor with Requester with EcologyMember 
{
  def notifyChanges(curState:SpaceState):Unit = {
    router ! CurrentPublicationState(curState)
  }
  
  def respondWithState(curState:SpaceState):Unit = {
    sender ! CurrentPublicationState(curState)
  }
}

object InPublicationStateActor {
  def actorProps(ecology:Ecology, spaceId:OID, router:ActorRef) = Props(classOf[InPublicationStateActor], ecology, spaceId, router)
}
