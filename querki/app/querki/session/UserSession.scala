package querki.session

import akka.actor._

import querki.ecology._
import querki.identity.UserId
import querki.notifications.{LoadInfo, UserInfo}
import querki.notifications.NotificationPersister._
import querki.spaces.SpacePersistenceFactory
import querki.util._

class UserSession(val ecology:Ecology, val userId:UserId) extends Actor with Stash with Requester 
  with TimeoutChild with EcologyMember 
{
  
  lazy val PersistenceFactory = interface[querki.spaces.SpacePersistenceFactory]
  lazy val UserEvolutions = interface[querki.evolutions.UserEvolutions]
  
  def timeoutConfig:String = "querki.userSession.timeout"
  
  lazy val notePersister = PersistenceFactory.getNotificationPersister(userId)
  
  override def preStart() = {
    notePersister ! LoadInfo
  }

  /**
   * The initial receive just handles setup, and then switches to mainReceive once it is ready:
   */
  def receive = {
    case UserInfo(id, version) => {
      // NOTE: this can take a long time! This is the point where we evolve the User to the
	  // current version:
	  UserEvolutions.checkUserEvolution(userId, version)
	  
	  notePersister.request(Load) {
	    case CurrentNotifications() => {
	      // TODO: store the Notifications in some table here
	      
	      // Okay, we're ready to roll:
	      unstashAll()
	      context.become(mainReceive)
	    }
	  }
    }
    
    case _ => stash()
  }
  
  def mainReceive:Receive = {
    case _ => ???
  }
}

object UserSession {
  // TODO: the following Props signature is now deprecated, and should be replaced (in Akka 2.2)
  // with "Props(classOf(Space), ...)". See:
  //   http://doc.akka.io/docs/akka/2.2.3/scala/actors.html
  def actorProps(ecology:Ecology, id:UserId):Props = Props(new UserSession(ecology, id))
}