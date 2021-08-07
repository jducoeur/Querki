package querki.publication

import akka.actor._
import akka.persistence._

import funcakka._
import org.querki.requester._

import querki.globals._
import querki.identity.User
import querki.identity.IdentityPersistence.UserRef
import querki.persistence.UseKryo
import querki.spaces.SpaceMessagePersistence.SpaceEvent

trait PublicationStateMessage

/**
 * Message that says to add the given Events to the Publication History. Responds with a
 * CurrentPublicationState.
 */
case class AddPublicationEvents(evts: List[SpaceEvent with UseKryo]) extends PublicationStateMessage

/**
 * Message from the SpaceCore to the InPublicationStateActor, saying that this Thing has been
 * Published, so we should drop the In-Publication version.
 */
case class ThingPublished(
  who: UserRef,
  thingId: OID
) extends PublicationStateMessage
case class PublishedAck()

class InPublicationStateActor(
  val ecology: Ecology,
  val id: OID,
  val router: ActorRef
) extends InPublicationStateCore
     with RealActorCore
     with PersistentActor
     with Requester
     with EcologyMember {

  def notifyChanges(curState: CurrentPublicationState): Unit = {
    router ! curState
  }

  def respondWithState(curState: CurrentPublicationState): Unit = {
    sender ! curState
  }

  def respondPublished(): Unit = sender ! PublishedAck()
}

object InPublicationStateActor {

  def actorProps(
    ecology: Ecology,
    spaceId: OID,
    router: ActorRef
  ) = Props(classOf[InPublicationStateActor], ecology, spaceId, router)
}
