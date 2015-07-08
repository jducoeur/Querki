package querki.session

import scala.concurrent.Future

import querki.globals._
import Implicits.execContext

import querki.notifications._
import querki.notifications.NotificationPersister._
import querki.values.RequestContext

class NotificationFunctionsImpl/*(info:AutowireParams)*/(notes:UserNotificationActor, rc:RequestContext)(implicit val ecology:Ecology)
  extends /*AutowireApiImpl(info, e) with*/ NotificationFunctions with EcologyMember
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
