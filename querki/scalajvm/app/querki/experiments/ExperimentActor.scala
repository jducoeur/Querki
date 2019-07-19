package querki.experiments

import akka.actor.{ActorRef, Stash, Actor, Props}
import akka.contrib.pattern.ReceivePipeline
import akka.event.LoggingReceive
import querki.api.ClientRequest
import querki.globals.{EcologyMember, OID, Ecology}
import querki.identity.User
import querki.publication.CurrentPublicationState
import querki.session.OldUserSpaceSession
import querki.session.messages.{GetActiveSessions, ActiveSessions}
import querki.spaces.SpaceEvolution
import querki.spaces.messages.{CurrentState, SpaceSubsystemRequest}
import querki.util.{QuerkiBootableActor, TimeoutChild, RoutingParent}
import querki.values.SpaceState

/**
  * An Actor that encapsulates a single Experiment.
  *
  * At least for the moment, this is both the Experiment itself and the parent of any OldUserSpaceSessions working in
  * that Experiment. This makes it both a TimeoutChild and RoutingParent -- does that work?
  */
private [experiments] class ExperimentActor(e: Ecology, val spaceId:OID, val spaceRouter:ActorRef)
  extends QuerkiBootableActor(e) with EcologyMember with ReceivePipeline with TimeoutChild with SpaceEvolution with RoutingParent[User]
{
  val id: OID = spaceId
  val timeoutConfig = "querki.experiment.timeout"

  var state: Option[SpaceState] = None

  def createChild(key:User):ActorRef = {
    // Sessions need a special dispatcher so they can use Stash. (Seriously? Unfortunate leakage in the Akka API.)
    context.actorOf(OldUserSpaceSession.actorProps(ecology, spaceId, key, spaceRouter, ActorRef.noSender, false).withDispatcher("session-dispatcher"), key.id.toString)
  }

  override def initChild(user:User, child:ActorRef) = {
    state.map(child ! CurrentState(_))
  }

  def bootReceive = {
    case msg@CurrentState(s, _) => {
      state = Some(s)
      doneBooting()
    }
  }

  def doReceive = LoggingReceive {
    /**
      * The Space has sent an updated State, so tell everyone about it.
      */
    case msg @ CurrentState(s, _) => {
      state = Some(s)
      children.foreach(session => session.forward(msg))
    }

    case GetActiveSessions => sender ! ActiveSessions(children.size)

    /**
      * Message to forward to a UserSpaceSession. Create the session, if needed.
      */
    case msg @ SpaceSubsystemRequest(rc, _, payload) => {
      payload match {
        case _ => routeToChild(rc.requesterOrAnon, msg)
      }
    }
    case msg @ ClientRequest(req, rc) => routeToChild(rc.requesterOrAnon, msg)
  }
}

object ExperimentActor {
  def actorProps(e: Ecology, spaceId: OID, router: ActorRef): Props =
    Props(classOf[ExperimentActor], e, spaceId, router)
}
