package querki.session

import akka.actor._
import akka.event.LoggingReceive
import akka.persistence._

import org.querki.requester._

import querki.globals._

import models.OID

import querki.api.{AutowireParams, ClientRequest}
import querki.ecology._
import querki.identity.{CollaboratorCache, IdentityId, PublicIdentity, UserId}
import querki.identity.skilllevel._
import querki.persistence._
import querki.time.DateTime
import querki.util.ClusterTimeoutChild
import querki.values.RequestContext

private[session] class UserSession(val ecology: Ecology)
  extends PersistentActor
     with Requester
     with EcologyMember
     with ClusterTimeoutChild {
  import UserSessionMessages._
  import UserSession._

  lazy val ApiInvocation = interface[querki.api.ApiInvocation]
  lazy val UserEvolutions = interface[querki.evolutions.UserEvolutions]
  lazy val UserAccess = interface[querki.identity.UserAccess]

  def timeoutConfig: String = "querki.userSession.timeout"

  lazy val userId: OID = OID(self.path.name)

  override def persistenceId = s"user$userId"

  /**
   * The current State of this User -- mainly a collection of persisted preferences.
   *
   * SkillLevel defaults to the "middle" level.
   */
  var currentState = UserState(querki.identity.skilllevel.MOIDs.SkillLevelStandardOID)

  lazy val collaborators = context.actorOf(CollaboratorCache.actorProps(ecology, userId))

  override def preStart() = {
    // Evolve the User if needed:
    // TODO: in principle this shouldn't happen in preStart, but it does make the code a lot
    // simpler:
    UserAccess.getUserVersion(userId).map(UserEvolutions.checkUserEvolution(userId, _))
    super.preStart()
  }

  def mkParams(rc: RequestContext) = AutowireParams(rc.requesterOrAnon, None, rc, this, sender)

  def setSkillLevel(level: OID) = {
    currentState = currentState.copy(level)
  }

  val receiveRecover: Receive = {

    case SetSkillLevel(level) => {
      setSkillLevel(level)
    }

    case SnapshotOffer(metadata, snap) => {
      snap match {
        case old: UserStateOld => {
          // TODO: this is basically dead experimental code, and UserStateOld can be removed
          // before terribly long:
          QLog.spew(s"Recovered an old UserState: $old")
        }
        case state: UserState => {
          currentState = state
        }
      }
    }

    case RecoveryCompleted => {
      // We don't currently need to do anything at the end of recovery
    }

  }

  val receiveCommand: Receive = LoggingReceive {
    case msg: GetCollaborators => collaborators.forward(msg)

    // We handle UserFunctions, maybe some other APIs down the road:
    case ClientRequest(req, rc) => {
      ApiInvocation.handleSessionRequest(req, mkParams(rc))
    }

    case FetchUserSessionInfo(uid) => {
      sender ! UserSessionInfo(currentState.skillLevel)
    }

    case msg @ SetSkillLevel(level) => {
      persist(msg) { saved =>
        setSkillLevel(level)
        sender ! SkillLevelAck
        saveSnapshot(currentState)
      }
    }
  }
}

object UserSessionMessages {

  sealed trait UserSessionMsg {
    def userId: UserId
  }

  /**
   * Fetches the people who share Spaces with this Identity.
   */
  case class GetCollaborators(
    userId: UserId,
    identityId: IdentityId,
    term: String
  ) extends UserSessionMsg
  case class Collaborators(acs: Iterable[PublicIdentity])

  case class FetchUserSessionInfo(userId: UserId) extends UserSessionMsg
  case class UserSessionInfo(skillLevel: SkillLevelId)

  case class SetSkillLevel(@KryoTag(1) level: OID) extends UseKryo
  case object SkillLevelAck
}

object UserSession {
  def actorProps(ecology: Ecology): Props = Props(classOf[UserSession], ecology).withDispatcher("session-dispatcher")

  /* ----------------------
   * INTERNAL API
   */

  /**
   * PERSISTENT: Represents the state of this User. We expect that this will grow -- remember that you grow
   * this by creating new variants of it, and translating the old ones to it.
   */
  case class UserState(@KryoTag(1) skillLevel: OID) extends UseKryo
  case class UserStateOld() extends UseKryo
}
