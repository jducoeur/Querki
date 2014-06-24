package querki.session

import akka.actor._
import akka.event.LoggingReceive

import querki.ecology._
import querki.identity.UserId
import querki.notifications.{CurrentNotifications, LoadInfo, UserInfo}
import querki.notifications.NotificationPersister.Load
import querki.spaces.SpacePersistenceFactory
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
      sender ! UserSessionInfo(_currentNotes.get)
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