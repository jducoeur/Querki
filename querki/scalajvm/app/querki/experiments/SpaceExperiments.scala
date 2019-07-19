package querki.experiments

import akka.actor.{ActorRef, Props}
import akka.contrib.pattern.ReceivePipeline
import models.OID
import querki.api.ClientRequest
import querki.ecology.Ecology
import querki.globals.OID
import querki.spaces.messages.{SpaceSubsystemRequest, CurrentState}
import querki.util.{QuerkiBootableActor, RoutingParent}
import querki.values.{SpaceState, RequestContext}

private [experiments] class SpaceExperiments(e:Ecology, val spaceId:OID, val spaceRouter:ActorRef)
  extends QuerkiBootableActor(e) with ReceivePipeline with RoutingParent[OID]
{
  def createChild(key: OID): ActorRef = {
    context.actorOf(ExperimentActor.actorProps(e, spaceId, spaceRouter))
  }

  override def initChild(experimentId: OID, child:ActorRef) = {
    state.map(child ! CurrentState(_))
  }

  var state: Option[SpaceState] = None

  def bootReceive = {
    case msg@CurrentState(s, _) => {
      state = Some(s)
      doneBooting()
    }
  }

  def routeBasedOnRC(rc: RequestContext, msg: Any): Unit = {
    for {
      metadata <- rc.metadataOpt
      experimentName <- metadata.experimentOpt
      experimentId <- OID.parseOpt(experimentName)
    }
      routeToChild(experimentId, msg)
  }

  def doReceive = {
    /**
      * The Space has sent an updated State, so tell everyone about it.
      */
    case msg @ CurrentState(s, _) => {
      state = Some(s)
      children.foreach(session => session.forward(msg))
    }

    /**
      * Message to forward to a UserSpaceSession. Create the session, if needed.
      */
    case msg @ SpaceSubsystemRequest(rc, _, payload) => {
      payload match {
        // TODO: what *should* we do with UserValues when I'm in an Experiment?
//        case p:querki.uservalues.PersistMessages.ExternallyExposed => persister.forward(msg)
        case _ => routeBasedOnRC(rc, msg)
      }
    }
    case msg @ ClientRequest(req, rc) => routeBasedOnRC(rc, msg)
  }
}

object SpaceExperiments {
  def actorProps(e:Ecology, spaceId:OID, spaceRouter:ActorRef): Props =
    Props(classOf[SpaceExperiments], e, spaceId, spaceRouter)
}
