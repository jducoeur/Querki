package querki.session

import akka.actor._
import akka.event.LoggingReceive

// TEMP: while hacking the timestamps:
import com.github.nscala_time.time.Imports._

import querki.ecology._
import querki.identity.UserId
import querki.notifications.{CurrentNotifications, EmptyNotificationId, LoadInfo, Notification, UserInfo}
import querki.notifications.NotificationPersister.Load
import querki.spaces.SpacePersistenceFactory
import querki.time.DateTime
import querki.util._

private [session] class UserSession(val ecology:Ecology, val userId:UserId) extends Actor with Stash with Requester 
  with TimeoutChild with EcologyMember 
{
  import UserSessionMessages._
  
  lazy val PersistenceFactory = interface[querki.spaces.SpacePersistenceFactory]
  lazy val UserEvolutions = interface[querki.evolutions.UserEvolutions]
  
  def timeoutConfig:String = "querki.userSession.timeout"
  
  lazy val notePersister = PersistenceFactory.getNotificationPersister(userId)

  var currentNotes:Seq[Notification] = Seq.empty
  
  var nextNoteId:Int = EmptyNotificationId
  
  // TODO: make this real:
  var _lastCheckedNotes:DateTime = DateTime.now - 1.month
  
  // How many of the Notifications are new since this User last looked at the Notifications Window?
  def numNewNotes:Int = {
    val newNotes = currentNotes.filter(note => note.sentTime.isAfter(_lastCheckedNotes) && !note.isRead)
    newNotes.size
  }
  
  override def preStart() = {
    notePersister ! LoadInfo
  }

  /**
   * The initial receive just handles setup, and then switches to mainReceive once it is ready:
   */
  def receive = LoggingReceive {
    case UserInfo(id, version) => {
      // NOTE: this can take a long time! This is the point where we evolve the User to the
	  // current version:
	  UserEvolutions.checkUserEvolution(userId, version)
	  
	  notePersister.request(Load) {
	    case notes:CurrentNotifications => {
	      currentNotes = notes.notes
	      
	      nextNoteId = 
	        if (currentNotes.isEmpty)
	          0
	        else
	          currentNotes.map(_.id).max + 1
	      
	      // Okay, we're ready to roll:
	      unstashAll()
	      context.become(mainReceive)
	    }
	  }
    }
    
    case msg:UserSessionMsg => stash()
  }
  
  def mainReceive:Receive = LoggingReceive {
    case FetchSessionInfo(_) => {
      // TODO: make this real
      sender ! UserSessionInfo(numNewNotes)
    }
    
    case NewNotification(_, noteRaw) => {
      // We decide what the actual Notification Id is:
      val note = noteRaw.copy(id = nextNoteId)
      nextNoteId += 1
      notePersister ! NewNotification(userId, note)
      
      currentNotes :+= note
    }
  }
}

object UserSessionMessages {
  sealed trait UserSessionMsg {
    def userId:UserId
  }
  
  case class FetchSessionInfo(userId:UserId) extends UserSessionMsg
  
  /**
   * Fire-and-forget message, telling this UserSession that they are receiving a new Notification.
   */
  case class NewNotification(userId:UserId, note:Notification) extends UserSessionMsg
}

object UserSession {
  // TODO: the following Props signature is now deprecated, and should be replaced (in Akka 2.2)
  // with "Props(classOf(Space), ...)". See:
  //   http://doc.akka.io/docs/akka/2.2.3/scala/actors.html
  def actorProps(ecology:Ecology, id:UserId):Props = Props(new UserSession(ecology, id)) 
}