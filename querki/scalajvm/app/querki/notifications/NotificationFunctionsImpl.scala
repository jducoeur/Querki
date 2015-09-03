package querki.notifications

import scala.concurrent.Future

import querki.api.{AutowireApiImpl, AutowireParams}
import querki.globals._
import querki.notifications.NotificationPersister._

/**
 * Handler for the NotificationFunctions API. Note that this is incestuous with UserNotificationActor,
 * which is the Requester that runs it -- they share the *same* copy of UserNotificationState. This is
 * technically legal, since this runs in the Actor's receive loop, but it ain't pretty. It might be
 * less scary to come up with an immutable way to deal with this.
 */
class NotificationFunctionsImpl(info:AutowireParams)(implicit e:Ecology)
  extends AutowireApiImpl(info, e) with NotificationFunctions
{
  lazy val ClientApi = interface[querki.api.ClientApi]
  lazy val IdentityAccess = interface[querki.identity.IdentityAccess]
  lazy val Notifications = interface[querki.notifications.Notifications]
  
  def doRoute(req:Request):Future[String] = route[NotificationFunctions](this)(req)
  
  lazy val notes = info.payload.get.asInstanceOf[UserNotificationState]
  
  def getRecentNotifications():Future[Seq[NotificationInfo]] = {
    // TODO: update lastNoteChecked. We probably should be refactoring all that Notification stuff into here.
    val identityIds = notes.currentNotes.map(_.sender)
    // This is a tad ornate, but necessary. getIdentities returns a Future, and we're then using it
    // to manipulate notes -- that is, the Requester. So it is required that getIdentities() get looped
    // back into the Requester's receive loop, and the result has to be re-Futurized.
    // TBD: can we come up with a more elegant approach for this?
    loopback(IdentityAccess.getIdentities(identityIds)) flatMap { identities =>
      try {
	      val noteFuts = notes.currentNotes.map { note =>
	        val sender = ClientApi.identityInfo(identities(note.sender))
	        Notifications.render(rc, note) map { rendered =>
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
	      }
        Future.sequence(noteFuts)
      } catch {
        case ex:Exception => { QLog.error("Exception in getRecentNotifications", ex); throw ex }
      }
    }
  }
  
  def numNewNotifications():Int = {
    // TODO: once we have machinery to mark notes as Read, we should filter on that here:
    val newNotes = notes.currentNotes.filter(note => (note.id > notes.lastNoteChecked)/* && !note.isRead*/)
    newNotes.size
  }
  
  def readThrough(id:NotificationId):Unit = {
    if (id > notes.lastNoteChecked) {
      notes.lastNoteChecked = id
      notes.notePersister ! UpdateLastChecked(notes.lastNoteChecked)
    }
  }
}
