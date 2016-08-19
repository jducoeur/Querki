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

/**
 * "Old" version of UserSession, suitable for running without Cassandra while we are
 * in transition. Only used if querki.cassandra.enabled = false.
 */
private [session] class OldUserSession(val ecology:Ecology)
  extends Requester with EcologyMember with ClusterTimeoutChild
{
  import UserSessionMessages._
  import UserSession._
  
  lazy val ApiInvocation = interface[querki.api.ApiInvocation]
  lazy val UserEvolutions = interface[querki.evolutions.UserEvolutions]
  lazy val UserAccess = interface[querki.identity.UserAccess]
  
  def timeoutConfig:String = "querki.userSession.timeout"
  
  lazy val userId:OID = OID(self.path.name)
  
  lazy val collaborators = context.actorOf(CollaboratorCache.actorProps(ecology, userId))
  
  override def preStart() = {
    // Evolve the User if needed:
    // TODO: in principle this shouldn't happen in preStart, but it does make the code a lot
    // simpler:
    UserAccess.getUserVersion(userId).map(UserEvolutions.checkUserEvolution(userId, _))
    super.preStart()
  }
  
  def mkParams(rc:RequestContext) = AutowireParams(rc.requesterOrAnon, None, rc, this, sender)

  val receive:Receive = LoggingReceive {
    case msg:GetCollaborators => collaborators.forward(msg)
    
    // We handle UserFunctions, maybe some other APIs down the road:
    case ClientRequest(req, rc) => {
      ApiInvocation.handleSessionRequest(req, mkParams(rc))
    }
    
    case FetchUserSessionInfo(uid) => {
      sender ! UserSessionInfo(querki.identity.skilllevel.MOIDs.SkillLevelAdvancedOID)
    }
    
    case msg @ SetSkillLevel(level) => {
      // In this version, we just don't handle this message yet
    }
  }
}

object OldUserSession {
  def actorProps(ecology:Ecology):Props = Props(classOf[OldUserSession], ecology).withDispatcher("session-dispatcher")
}
