package querki.spaces

import akka.actor._
import akka.event.LoggingReceive

import org.querki.requester._

import models.OID
import querki.admin.SpaceTimingActor
import querki.api.ClientRequest
import querki.conversations.messages.{ActiveThings, GetActiveThings}
import querki.globals._
import querki.history.SpaceHistory
import querki.photos.PhotoUploadActor
import querki.publication._
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
private[spaces] class SpaceRouter(e: Ecology) extends Actor with EcologyMember with Requester with ClusterTimeoutChild {
  lazy val AdminOps = interface[querki.admin.AdminOps]
  lazy val Conversations = interface[querki.conversations.Conversations]
  lazy val persistenceFactory = interface[SpacePersistenceFactory]

  implicit val ecology = e

  lazy val tracing = TracingSpace(spaceId, "SpaceRouter: ")

  /**
   * This is the hard "None Shall Pass". If this is set on the Space, all messages will be rejected with an error.
   *
   * Obviously, use this only in extreme cases, generally when the Space is causing damage to the system as a whole.
   */
  lazy val spaceBlocked = Config.getBoolean(s"querki.debug.space.${spaceId.toString}.block", false)

  lazy val spaceId: OID = OID(self.path.name)

  val isBeingTimed = AdminOps.isTimedSpace(spaceId)

  // How long we can be inactive before timing out this entire hive:
  def timeoutConfig: String = {
    // QI.7w4gfs3: to temporarily ameliorate the headache of how long it is taking for the Playtest Space to load,
    // we are allowing it to have a much-longer-than-usual timeout:
    val spaceSpecificConfig = s"querki.debug.space.${spaceId.toString}.timeout"
    if (context.system.settings.config.hasPath(spaceSpecificConfig))
      spaceSpecificConfig
    else
      "querki.space.timeout"
  }

  // The components of the troupe. All are started when needed, but several deliberately reference space, because
  // they can't fully start until they receive the initial CurrentState message.
  lazy val timingOpt =
    if (isBeingTimed)
      Some(context.actorOf(SpaceTimingActor.actorProps(ecology), "SpaceTiming"))
    else
      None

  lazy val space = {
    tracing.trace("Booting PersistentSpaceActor")
    context.actorOf(
      PersistentSpaceActor.actorProps(ecology, persistenceFactory, self, spaceId, timingOpt.isDefined),
      "Space"
    )
  }

  lazy val conversations = {
    tracing.trace("Booting Conversations")
    bootSpaceActor()
    context.actorOf(Conversations.conversationsManagerProps(self))
  }

  lazy val sessions = {
    tracing.trace("Booting Sessions")
    bootSpaceActor()
    context.actorOf(UserSpaceSessions.actorProps(ecology, spaceId, self, timingOpt.isDefined), "Sessions")
  }

  lazy val members = {
    tracing.trace("Booting SpaceMembers")
    bootSpaceActor()
    context.actorOf(SpaceMembersActor.actorProps(ecology, spaceId, self), "Members")
  }

  lazy val history = {
    tracing.trace("Booting SpaceHistory")
    context.actorOf(SpaceHistory.actorProps(ecology, spaceId, self))
  }

  lazy val publication = {
    tracing.trace("Booting Publication")
    context.actorOf(PublicationActor.actorProps(ecology, spaceId, self), "Publication")
  }

  lazy val publicationStateActor = {
    tracing.trace("Booting InPublicationState")
    context.actorOf(InPublicationStateActor.actorProps(ecology, spaceId, self), "InPubState")
  }

  // This function just references the SpaceActor, so that it will boot and send CurrentState.
  // TODO: this is a dreadfully brittle approach. How can we restructure it to be more sensible
  // and robust?
  def bootSpaceActor() = {
    val dummy = space
    val dummy2 = publicationStateActor
  }

  var _state: Option[SpaceState] = None
  var _pubState: Option[CurrentPublicationState] = None

  def receive = LoggingReceive {

    /**
     * This Space is causing severe problems, so we have turned it off.
     *
     * THIS MUST COME FIRST!
     */
    case _ if spaceBlocked => {
      // All roads lead to errors for the time being:
      tracing.trace(s"Got a request for blocked Space")
      sender ! SpaceBlocked(new PublicException("Space.blocked"))
    }

    /**
     * The Space has sent an updated State, so tell everyone about it.
     */
    case msg @ CurrentState(curState, _) => {
      tracing.trace(s"publishing CurrentState ${curState.version}")
      _state = Some(curState)
      conversations.forward(msg)
      sessions.forward(msg)
      members.forward(msg)
    }

    case msg: CurrentPublicationState => {
      tracing.trace(s"publishing CurrentPublicationState")
      _pubState = Some(msg)
      sessions.forward(msg)
      space.forward(msg)
    }

    /**
     * Admin has asked all of the Spaces to give a quick status report.
     */
    case GetSpacesStatus(requester) => {
      tracing.trace(s"GetSpacesStatus")
      _state.map { state =>
        for {
          ActiveThings(nConvs) <- conversations.request(GetActiveThings)
          ActiveSessions(nSessions) <- sessions.request(GetActiveSessions)
        } sender ! SpaceStatus(spaceId, state.displayName, nConvs, nSessions)
      }
    }

    // Messages for the various subsystems get routed based on the payload type:
    case msg @ SpaceSubsystemRequest(_, _, payload) => {
      tracing.trace(s"SpaceSubsystemRequest(${payload.getClass.getSimpleName})")
      payload match {
        case p: querki.conversations.messages.ConversationMessage => conversations.forward(msg)
        case p: querki.session.messages.SessionMessage            => sessions.forward(msg)
        // HACK: messages heading for the User Value Persister:
        case p: querki.uservalues.PersistMessages.ExternallyExposed => sessions.forward(msg)
        case p: SpaceMembersBase                                    => members.forward(msg)
        case p: SpaceTimingActor.SpaceTimingMsg                     => timingOpt.map(_.forward(msg))
        case p: PublicationCommands.PublicationCommand              => publication.forward(p)
        case p: SpaceHistory.HistoryMessage                         => history.forward(p)
      }
    }

    // Message for a Session:
    case req: ClientRequest => {
      tracing.trace(s"forwarding ClientRequest(${req.req.path})")
      sessions.forward(req)
    }

    // Request for an upload actor under this Space. We create it as part of the troupe, but it's
    // basically anonymous after creation:
    case msg: BeginProcessingPhoto => {
      tracing.trace(s"booting PhotoUploadActor")
      _state.map { state =>
        val worker = context.actorOf(PhotoUploadActor.actorProps(ecology, state, self))
        worker.forward(msg)
        sender ! worker
      }
      // TODO: this should return some sort of serious error if _state is empty: that shouldn't be possible.
    }

    // Messages for the SpaceHistory:
    case msg: SpaceHistory.HistoryMessage => {
      tracing.trace(s"forwarding ${msg.getClass.getSimpleName}")
      history.forward(msg)
    }

    // Messages for the SpaceTimingActor, if one is running for this Space:
    case msg: SpaceTimingActor.SpaceTimingMsg => {
      tracing.trace(s"forwarding ${msg.getClass.getSimpleName}")
      timingOpt.map(_.forward(msg))
    }

    case msg: PublicationCommands.PublicationCommand => {
      tracing.trace(s"SpaceRouter forwarding ${msg.getClass.getSimpleName}")
      publication.forward(msg)
    }

    case msg: PublicationStateMessage => {
      tracing.trace(s"forwarding ${msg.getClass.getSimpleName}")
      publicationStateActor.forward(msg)
    }

    case msg: ShutdownSpace => {
      tracing.trace(s"Shutting down the Space")
      sender ! ShutdownAck
      self ! querki.util.Shutdown
    }

    // Message for the Space:
    case msg: CreateSpace => {
      tracing.trace("Creating the Space")
      space.forward(msg)
    }
    // FALLBACK -- NOTHING SHOULD GO BELOW HERE:
    case msg: SpaceMessage => {
      tracing.trace(s"forwarding ${msg.getClass.getSimpleName}")
      space.forward(msg)
    }
  }
}

object SpaceRouter {
  def actorProps(ecology: Ecology): Props = Props(classOf[SpaceRouter], ecology)
}
