package querki.session

import scala.concurrent.Future
import akka.actor._
import akka.event.LoggingReceive

import upickle._
import autowire._

import org.querki.requester._

import querki.globals._
import Implicits.execContext

import querki.notifications._
import querki.notifications.NotificationPersister._
import querki.values.RequestContext

import messages.{ClientRequest, ClientResponse}

// TODO: this is still much too incestuous with UserSession per se. Possibly we should pull most of the rest
// of the guts of UserSession into here, and leave UserSession as a rump router? We should likely extract all
// of the state into a single state class, which we could pass into NotificationFunctionsImpl instead of
// a pointer to this.
trait UserNotifications extends autowire.Server[String, upickle.Reader, upickle.Writer] with EcologyMember
{ self:Actor with Stash with Requester =>
  
  def userId:OID
  def ecology:Ecology
  
  import UserSessionMessages._
  
  lazy val PersistenceFactory = interface[querki.spaces.SpacePersistenceFactory]

  lazy val notePersister = PersistenceFactory.getNotificationPersister(userId)
  
  // Autowire functions
  def write[Result: Writer](r: Result) = upickle.write(r)
  def read[Result: Reader](p: String) = upickle.read[Result](p)
  
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
  
  def initNotes() = {
    notePersister.requestFor[CurrentNotifications](Load) foreach { notes =>
	  currentNotes = notes.notes.sortBy(_.id).reverse
	    
	  // Okay, we're ready to roll:
	  self ! InitComplete
	}
  }
  
  def notificationMessageReceive:Receive = LoggingReceive (handleRequestResponse orElse {
    
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
    
    case UserSessionClientRequest(_, ClientRequest(req, rc)) => {
      req.path(2) match {
        case "NotificationFunctions" => {
          // route() is asynchronous, so we need to store away the sender!
          val senderSaved = sender
          val handler = new NotificationFunctionsImpl(this, rc)
          route[NotificationFunctions](handler)(req).foreach { result =>
            senderSaved ! ClientResponse(result)
          }          
        }
      }
    }    
  })
}

class NotificationFunctionsImpl(notes:UserNotifications, rc:RequestContext)
  extends NotificationFunctions with EcologyMember
{
  lazy val ClientApi = interface[querki.api.ClientApi]
  lazy val IdentityAccess = interface[querki.identity.IdentityAccess]
  lazy val Notifications = interface[querki.notifications.Notifications]
  
  def getRecentNotifications():Future[Seq[NotificationInfo]] = {
    // TODO: update lastNoteChecked. We probably should be refactoring all that Notification stuff into here.
    val identityIds = notes.currentNotes.map(_.sender)
    IdentityAccess.getIdentities(identityIds).map { identities =>
      try {
	      notes.currentNotes.map { note =>
	        val sender = ClientApi.identityInfo(identities(note.sender))
	        val rendered = Notifications.render(rc, note)
	        NotificationInfo(
	          note.id,
	          sender,
	          note.spaceId.map(_.toThingId.toString).getOrElse(""),
	          note.thingId.map(_.toThingId.toString).getOrElse(""),
	          // TODO: This should really be an implicit conversion:
	          note.sentTime.getMillis,
	          rendered,
	          note.isRead,
	          note.isDeleted
	        )
	      }
      } catch {
        case ex:Exception => { QLog.error("Exception in getRecentNotifications", ex); throw ex }
      }
    }
  }
  
  def numNewNotifications():Int = {
    notes.numNewNotes
  }
  
  def readThrough(id:NotificationId):Unit = {
    if (id > notes.lastNoteChecked) {
      notes.lastNoteChecked = id
      notes.notePersister ! UpdateLastChecked(notes.lastNoteChecked)
    }
  }
}
