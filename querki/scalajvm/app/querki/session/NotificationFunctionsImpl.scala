package querki.session

import scala.concurrent.Future
import akka.actor._
import akka.event.LoggingReceive

import querki.globals._
import Implicits.execContext

import querki.notifications._
import querki.notifications.NotificationPersister._
import querki.util.Requester

trait NotificationFunctionsImpl extends NotificationFunctions with UserSessionApiImpl { self:Actor with Stash with Requester =>
  import UserSessionMessages._
  
  lazy val ClientApi = interface[querki.api.ClientApi]
  lazy val IdentityAccess = interface[querki.identity.IdentityAccess]
  lazy val Notifications = interface[querki.notifications.Notifications]
  def PersistenceFactory:querki.spaces.SpacePersistenceFactory

  lazy val notePersister = PersistenceFactory.getNotificationPersister(userId)
  
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
    notePersister.request(Load) {
	  case notes:CurrentNotifications => {
	    currentNotes = notes.notes.sortBy(_.id).reverse
	    
	    // Okay, we're ready to roll:
	    self ! InitComplete
	  }
	}
  }
  
  def notificationMessageReceive:Receive = LoggingReceive {
    
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
    
  }
  
  def getRecentNotifications():Future[Seq[NotificationInfo]] = {
    // TODO: update lastNoteChecked. We probably should be refactoring all that Notification stuff into here.
    val identityIds = currentNotes.map(_.sender)
    // Due to the presence of Futures, we have to store this away:
    val theRc = rc
    IdentityAccess.getIdentities(identityIds).map { identities =>
      try {
	      currentNotes.map { note =>
	        val sender = ClientApi.identityInfo(identities(note.sender))
	        val rendered = Notifications.render(theRc, note)
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
    numNewNotes
  }
  
  def readThrough(id:NotificationId):Unit = {
    if (id > lastNoteChecked) {
      lastNoteChecked = id
      notePersister ! UpdateLastChecked(lastNoteChecked)
    }
  }
}
