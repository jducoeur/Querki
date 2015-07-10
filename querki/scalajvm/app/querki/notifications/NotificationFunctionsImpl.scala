package querki.notifications

import scala.concurrent.Future

import querki.api.{AutowireApiImpl, AutowireParams}
import querki.globals._
import querki.notifications.NotificationPersister._

/**
 * Handler for the NotificationFunctions API. Note that this is *deeply* incestuous with UserNotificationActor,
 * which is the Requester that runs it!
 */
class NotificationFunctionsImpl(info:AutowireParams)(implicit e:Ecology)
  extends AutowireApiImpl(info, e) with NotificationFunctions
{
  lazy val ClientApi = interface[querki.api.ClientApi]
  lazy val IdentityAccess = interface[querki.identity.IdentityAccess]
  lazy val Notifications = interface[querki.notifications.Notifications]
  
  def doRoute(req:Request):Future[String] = route[NotificationFunctions](this)(req)
  
  // TODO: this is an ugly workaround for the fact that NotificationFunctionalImpl and UserNotificationActor needs to
  // share a cache. What would be a better way to do this?
  lazy val notes = requester.asInstanceOf[UserNotificationActor]
  
  def getRecentNotifications():Future[Seq[NotificationInfo]] = {
    // TODO: update lastNoteChecked. We probably should be refactoring all that Notification stuff into here.
    val identityIds = notes.currentNotes.map(_.sender)
    // This is a tad ornate, but necessary. getIdentities returns a Future, and we're then using it
    // to manipulate notes -- that is, the Requester. So it is required that getIdentities() get looped
    // back into the Requester's receive loop, and the result has to be re-Futurized.
    // TBD: can we come up with a more elegant approach for this?
    requestFuture[Seq[NotificationInfo]] { implicit promise =>
      loopback(IdentityAccess.getIdentities(identityIds)) foreach { identities =>
        try {
  	      promise.success(notes.currentNotes.map { note =>
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
  	      })
        } catch {
          case ex:Exception => { QLog.error("Exception in getRecentNotifications", ex); throw ex }
        }
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
