package querki.session

import akka.actor._
import akka.event.LoggingReceive

// TEMP: while hacking the timestamps:
import com.github.nscala_time.time.Imports._

import querki.globals._
import Implicits.execContext

import querki.ecology._
import querki.identity.{CollaboratorCache, IdentityId, PublicIdentity, UserId}
import querki.notifications.{CurrentNotifications, EmptyNotificationId, LoadInfo, Notification, NotificationFunctions, UpdateLastChecked, UserInfo}
import querki.notifications.NotificationPersister.Load
import querki.time.DateTime
import querki.util.{Requester, TimeoutChild}
import querki.values.RequestContext

import messages.ClientRequest

private [session] class UserSession(val ecology:Ecology, val userId:UserId) extends Actor with Stash with Requester 
  with TimeoutChild with EcologyMember with UserNotifications
{
  import UserSessionMessages._
  
  lazy val UserEvolutions = interface[querki.evolutions.UserEvolutions]
  
  def timeoutConfig:String = "querki.userSession.timeout"
  
  lazy val collaborators = context.actorOf(CollaboratorCache.actorProps(ecology, userId))
  
  override def preStart() = {
    // TODO: this shouldn't be going through the NotificationPersister:
    notePersister ! LoadInfo
  }

  /**
   * The initial receive just handles setup, and then switches to mainReceive once it is ready:
   */
  def receive = LoggingReceive {
    case UserInfo(id, version, lastChecked) => {
      lastNoteChecked = lastChecked
      
      // NOTE: this can take a long time! This is the point where we evolve the User to the
	  // current version:
	  UserEvolutions.checkUserEvolution(userId, version)
	  
	  // This will send InitComplete when it is done:
	  initNotes()
    }
    
    case InitComplete => {
      context.become(mainReceive)
      unstashAll()
    }

    // Hold everything else off until we've created them all:
    case msg:UserSessionMsg => stash()
  }
  
  def mainReceive:Receive = notificationMessageReceive orElse LoggingReceive {
    case FetchSessionInfo(_) => {
      // TODO: make this real
      sender ! UserSessionInfo(numNewNotes)
    }
    
    case msg:GetCollaborators => collaborators.forward(msg)
  }
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
  
  /**
   * Fetches the recent Notifications for this user.
   */
  case class GetRecent(userId:UserId) extends UserSessionMsg {
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
  // TODO: the following Props signature is now deprecated, and should be replaced (in Akka 2.2)
  // with "Props(classOf(Space), ...)". See:
  //   http://doc.akka.io/docs/akka/2.2.3/scala/actors.html
  def actorProps(ecology:Ecology, id:UserId):Props = Props(new UserSession(ecology, id)) 
}