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
import querki.notifications.{CurrentNotifications, EmptyNotificationId, LoadInfo, Notification, NotificationFunctions, UpdateLastChecked}
import querki.notifications.NotificationPersister.Load
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
  
  lazy val notifications = context.actorOf(UserNotificationActor.actorProps(userId, ecology))
  
  override def preStart() = {
    // Kick the UserNotifications to life.
    // TODO: this should go away, and the Notification should become its own thing:
    notifications
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
    case msg:FetchSessionInfo => notifications.forward(msg)
    
    case msg:NewNotification => notifications.forward(msg)
    
    case msg:GetCollaborators => collaborators.forward(msg)
    
    // TODO: this is wrong! It is just temporary:
    case msg @ UserSessionClientRequest(_, ClientRequest(req, rc)) => {
      req.path(2) match {
        case "NotificationFunctions" => notifications.forward(msg)
        // TODO: handle stuff natively registered under UserSession:
        case _ => ???
      }
    }  
  })
}

object UserSessionMessages {
  sealed trait UserSessionMsg {
    def userId:UserId
    // This is a somewhat clumsy mechanism to deal with the fact that you can't call copy() on a trait.
    // TODO: this kinda sucks. How can we restructure these to make it suck less?
    def copyTo(userId:UserId):UserSessionMsg
  }
  
  case object InitComplete
  
  case class FetchSessionInfo(userId:UserId) extends UserSessionMsg {
    def copyTo(userId:UserId) = copy(userId = userId)
  }
  
  /**
   * Fire-and-forget message, telling this UserSession that they are receiving a new Notification.
   */
  case class NewNotification(userId:UserId, note:Notification) extends UserSessionMsg {
    def copyTo(userId:UserId) = copy(userId = userId)
  }
  case class RecentNotifications(notes:Seq[Notification])
  
  /**
   * Fetches the people who share Spaces with this Identity.
   */
  case class GetCollaborators(userId:UserId, identityId:IdentityId, term:String) extends UserSessionMsg {
    def copyTo(userId:UserId) = copy(userId = userId)    
  }
  case class Collaborators(acs:Iterable[PublicIdentity])

  /**
   * An RPC request from the Client.
   */
  case class UserSessionClientRequest(userId:UserId, req:ClientRequest) extends UserSessionMsg {
    def copyTo(userId:UserId) = copy(userId = userId)
  }
}

object UserSession {
  def actorProps(ecology:Ecology):Props = Props(classOf[UserSession], ecology).withDispatcher("session-dispatcher")
}