package querki.session

import akka.actor._
import akka.event.LoggingReceive

// TEMP: while hacking the timestamps:
import com.github.nscala_time.time.Imports._

import upickle._
import autowire._

import querki.globals._
import Implicits.execContext

import querki.ecology._
import querki.identity.{CollaboratorCache, IdentityId, PublicIdentity, UserId}
import querki.notifications.{CurrentNotifications, EmptyNotificationId, LoadInfo, Notification, NotificationFunctions, UpdateLastChecked, UserInfo}
import querki.notifications.NotificationPersister.Load
import querki.spaces.SpacePersistenceFactory
import querki.time.DateTime
import querki.util.{Requester, TimeoutChild}
import querki.values.RequestContext

import messages.{ClientRequest, ClientResponse}

/**
 * The various UserSession-focused implementations should inherit from this. They then get mixed into
 * UserSpaceSession, and use this trait to access the critical contextual info. 
 */
trait UserSessionApiImpl extends EcologyMember {
  /**
   * The User who is making this request.
   */
  def userId:UserId
  
  def rc:RequestContext
  
  // TODO: all of this Notification code should get refactored out into NotificationFunctionsImpl:
  def currentNotes:Seq[Notification]
  var lastNoteChecked:Int
  def numNewNotes:Int
  def notifyNotePersister():Unit
}

private [session] class UserSession(val ecology:Ecology, val userId:UserId) extends Actor with Stash with Requester 
  with TimeoutChild with EcologyMember 
  with autowire.Server[String, upickle.Reader, upickle.Writer]
  with NotificationFunctionsImpl
{
  import UserSessionMessages._
  
  lazy val PersistenceFactory = interface[querki.spaces.SpacePersistenceFactory]
  lazy val UserEvolutions = interface[querki.evolutions.UserEvolutions]
  
  def timeoutConfig:String = "querki.userSession.timeout"
  
  lazy val notePersister = PersistenceFactory.getNotificationPersister(userId)
  
  lazy val collaborators = context.actorOf(CollaboratorCache.actorProps(ecology, userId))

  // This is kept in most-recent-first order:
  var currentNotes:Seq[Notification] = Seq.empty
    
  var lastNoteChecked:Int = 0
  
  // How many of the Notifications are new since this User last looked at the Notifications Window?
  def numNewNotes:Int = {
    // TODO: once we have machinery to mark notes as Read, we should filter on that here:
    val newNotes = currentNotes.filter(note => (note.id > lastNoteChecked)/* && !note.isRead*/)
    newNotes.size
  }
  
  def currentMaxNote = {
    if (currentNotes.isEmpty)
      0
    else
      currentNotes.map(_.id).max    
  }
  def nextNoteId:Int = currentMaxNote + 1
  
  var _currentRc:Option[RequestContext] = None
  def rc = _currentRc.get
  def withRc[R](rc:RequestContext)(f: => R):R = {
    _currentRc = Some(rc)
    try {
      f
    } finally {
      _currentRc = None
    }
  }
  
  // Autowire functions
  def write[Result: Writer](r: Result) = upickle.write(r)
  def read[Result: Reader](p: String) = upickle.read[Result](p)
  
  override def preStart() = {
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
	  
	  notePersister.request(Load) {
	    case notes:CurrentNotifications => {
	      currentNotes = notes.notes.sortBy(_.id).reverse
	      
	      // Okay, we're ready to roll:
	      unstashAll()
	      context.become(mainReceive)
	    }
	  }
    }

    // Hold everything else off until we've created them all:
    case msg:UserSessionMsg => stash()
  }
  
  // TODO: this is basically a hacked workaround, and should go away once the Notification stuff all\
  // moved into NotificationFunctionsImpl:
  def notifyNotePersister() = {
	notePersister ! UpdateLastChecked(lastNoteChecked)    
  }
  
  def mainReceive:Receive = LoggingReceive {
    case FetchSessionInfo(_) => {
      // TODO: make this real
      sender ! UserSessionInfo(numNewNotes)
    }
    
    case NewNotification(_, noteRaw) => {
      // We decide what the actual Notification Id is:
      val note = noteRaw.copy(id = nextNoteId)
      
      notePersister ! NewNotification(userId, note)
      
      currentNotes = note +: currentNotes
    }
    
    case GetRecent(_) => {
      sender ! RecentNotifications(currentNotes)
      lastNoteChecked = currentMaxNote
      notePersister ! UpdateLastChecked(lastNoteChecked)
    }
    
    case msg:GetCollaborators => collaborators.forward(msg)
    
    case UserSessionClientRequest(_, ClientRequest(req, rc)) => {
      withRc(rc) {
	      req.path(2) match {
	        case "NotificationFunctions" => {
	          // route() is asynchronous, so we need to store away the sender!
	          val senderSaved = sender
	          route[NotificationFunctions](this)(req).foreach { result =>
	            senderSaved ! ClientResponse(result)
	          }          
	        }
	      }
      }
    }
  }
}

object UserSessionMessages {
  sealed trait UserSessionMsg {
    def userId:UserId
    // This is a somewhat clumsy mechanism to deal with the fact that you can't call copy() on a trait.
    // TODO: this kinda sucks. How can we restructure these to make it suck less?
    def copyTo(userId:UserId):UserSessionMsg
  }
  
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