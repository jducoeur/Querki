package querki.session

import scala.concurrent.Future

import querki.globals._
import Implicits.execContext

import querki.notifications._

trait NotificationFunctionsImpl extends NotificationFunctions with UserSessionApiImpl {
  
  lazy val ClientApi = interface[querki.api.ClientApi]
  lazy val IdentityAccess = interface[querki.identity.IdentityAccess]
  lazy val Notifications = interface[querki.notifications.Notifications]
  
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
      notifyNotePersister()
    }
  }
}
