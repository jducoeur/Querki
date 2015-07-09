package querki.session

import akka.actor._
import akka.event.LoggingReceive

// TEMP: while hacking the timestamps:
import com.github.nscala_time.time.Imports._

import org.querki.requester._

import querki.globals._
import Implicits.execContext

import models.OID

import querki.ecology._
import querki.identity.{CollaboratorCache, IdentityId, PublicIdentity, UserId}
import querki.time.DateTime
import querki.util.ClusterTimeoutChild
import querki.values.RequestContext

import messages.ClientRequest

private [session] class UserSession(val ecology:Ecology) extends Actor with Stash
  with Requester with EcologyMember with ClusterTimeoutChild
{
  import UserSessionMessages._
  
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

  /**
   * The initial receive just handles setup, and then switches to mainReceive once it is ready:
   */
  def receive = LoggingReceive (handleRequestResponse orElse {
    case msg:GetCollaborators => collaborators.forward(msg)
  })
}

object UserSessionMessages {
  sealed trait UserSessionMsg {
    def userId:UserId
    // This is a somewhat clumsy mechanism to deal with the fact that you can't call copy() on a trait.
    // TODO: this kinda sucks. How can we restructure these to make it suck less?
    def copyTo(userId:UserId):UserSessionMsg
  }
  
  /**
   * Fetches the people who share Spaces with this Identity.
   */
  case class GetCollaborators(userId:UserId, identityId:IdentityId, term:String) extends UserSessionMsg {
    def copyTo(userId:UserId) = copy(userId = userId)    
  }
  case class Collaborators(acs:Iterable[PublicIdentity])
}

object UserSession {
  def actorProps(ecology:Ecology):Props = Props(classOf[UserSession], ecology).withDispatcher("session-dispatcher")
}