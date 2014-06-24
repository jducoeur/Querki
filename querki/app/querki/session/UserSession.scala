package querki.session

import akka.actor._
import akka.event.LoggingReceive

// TEMP: while hacking the timestamps:
import com.github.nscala_time.time.Imports._

import querki.ecology._
import querki.identity.UserId
import querki.notifications.{CurrentNotifications, LoadInfo, UserInfo}
import querki.notifications.NotificationPersister.Load
import querki.spaces.SpacePersistenceFactory
import querki.time.DateTime
import querki.util._

class UserSession(val ecology:Ecology, val userId:UserId) extends Actor with Stash with Requester 
  with TimeoutChild with EcologyMember 
{
  import UserSession._
  
  lazy val PersistenceFactory = interface[querki.spaces.SpacePersistenceFactory]
  lazy val UserEvolutions = interface[querki.evolutions.UserEvolutions]
  
  def timeoutConfig:String = "querki.userSession.timeout"
  
  lazy val notePersister = PersistenceFactory.getNotificationPersister(userId)
  
  var _currentNotes:Option[CurrentNotifications] = None
  
  // TODO: make this real:
  var _lastCheckedNotes:DateTime = DateTime.now - 1.month
  
  // How many of the Notifications are new since this User last looked at the Notifications Window?
  def numNewNotes:Int = {
    _currentNotes match {
      case Some(curr) => {
        val newNotes = curr.notes.filter(note => note.sentTime.isAfter(_lastCheckedNotes) && !note.isRead)
        newNotes.size
      }
      case None => 0
    }
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
	      // TODO: store the Notifications in some table here
	      _currentNotes = Some(notes)
	      
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
  }
}

object UserSession {
  // TODO: the following Props signature is now deprecated, and should be replaced (in Akka 2.2)
  // with "Props(classOf(Space), ...)". See:
  //   http://doc.akka.io/docs/akka/2.2.3/scala/actors.html
  def actorProps(ecology:Ecology, id:UserId):Props = Props(new UserSession(ecology, id))
  
  sealed trait UserSessionMsg {
    def userId:UserId
  }
  
  case class FetchSessionInfo(userId:UserId) extends UserSessionMsg
}