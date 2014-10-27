package controllers

import scala.concurrent.ExecutionContext.Implicits._

import querki.notifications.CurrentNotifications

class NotificationController extends ApplicationBase {
  
  lazy val UserSession = interface[querki.session.Session]

  // TODO: this is old UI, and should go away
  def showNotifications = withUser(true) { rc =>
    // IMPORTANT: this is a comprehension of Futures!
    for {
      recentNotes <- UserSession.getNotifications(rc.requesterOrAnon)
      identityIds = recentNotes.notes.map(_.sender)
      identities <- IdentityAccess.getIdentities(identityIds)
    }
      yield Ok(views.html.showNotifications(rc, recentNotes, identities))
  }
  
  ///////////////////////////
  //
  // New API
  //

  /**
   * TBD: this might belong as an Autowire entry point in UserSession? (Not UserSpaceSession,
   * though.)
   */
  def numNotifications = withUser(true) { rc =>
    Ok(rc.numNotifications.toString)
  }
}
