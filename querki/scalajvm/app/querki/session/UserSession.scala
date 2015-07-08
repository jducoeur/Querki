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
import querki.notifications.{CurrentNotifications, EmptyNotificationId, LoadInfo, Notification, NotificationFunctions, UpdateLastChecked, UserInfo}
import querki.notifications.NotificationPersister.Load
import querki.time.DateTime
import querki.util.ClusterTimeoutChild
import querki.values.RequestContext

import messages.ClientRequest

private [session] class UserSession(val ecology:Ecology) extends Actor with Stash
  with Requester with EcologyMember with ClusterTimeoutChild with ImplCacheProvider
{
  import UserSessionMessages._
  
  lazy val UserEvolutions = interface[querki.evolutions.UserEvolutions]
  
  def timeoutConfig:String = "querki.userSession.timeout"
  
  lazy val userId:OID = OID(self.path.name)
  
  lazy val collaborators = context.actorOf(CollaboratorCache.actorProps(ecology, userId))
  
  lazy val notifications = context.actorOf(UserNotifications.actorProps(userId, ecology, self))
  
  override def preStart() = {
    // Kick the UserNotifications to life. That actually fires up fully first, and we finish
    // getting this ready when it sends us the UserInfo:
    notifications
    super.preStart()
  }

  /**
   * The initial receive just handles setup, and then switches to mainReceive once it is ready:
   */
  def receive = LoggingReceive (handleRequestResponse orElse {
    // TODO: this gets sent from the UserNotifications Actor when it is ready. That is rather
    // broken conceptually -- we should move towards something better decoupled.
    case UserInfo(id, version, lastChecked) => {
      QLog.spew("UserSession got the UserInfo")
      // NOTE: this can take a long time! This is the point where we evolve the User to the
  	  // current version:
	    UserEvolutions.checkUserEvolution(userId, version)
      context.become(mainReceive)
      unstashAll()
    }

    // Hold everything else off until we've created them all:
    case msg:UserSessionMsg => stash()
  })
  
  def mainReceive:Receive = LoggingReceive (handleRequestResponse orElse {
    case msg:FetchSessionInfo => notifications.forward(msg)
    
    case msg:NewNotification => notifications.forward(msg)
    
    case msg:GetCollaborators => collaborators.forward(msg)
    
    // TODO: this is wrong! It is just temporary:
    case msg:UserSessionClientRequest => notifications.forward(msg)
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