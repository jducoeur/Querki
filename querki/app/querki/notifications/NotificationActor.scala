package querki.notifications

import akka.actor._

import querki.ecology._
import querki.identity.{PublicIdentity, User, UserCacheMessages, UserLevel}
import querki.session.UserSessionMessages
import querki.util._

/**
 * For the moment, this is a singleton that manages Notifications. That *clearly* isn't right:
 * this should really be the root of a bunch of Workers that do any actual work.
 * 
 * TODO: once I understand this code better, refactor it into more Actors, to remove this
 * bottleneck.
 */
private[notifications] class NotificationActor(val ecology:Ecology) extends Actor with EcologyMember with Requester {
  import NotificationActor._
  
  lazy val IdentityAccess = interface[querki.identity.IdentityAccess]
  lazy val SessionAccess = interface[querki.session.Session]
  
  lazy val sessionManager = SessionAccess.sessionManager
  lazy val userCache = IdentityAccess.userCache
  
  def receive = {
    case msg @ SendNotification(req, as, recipients, note) => {
      
      val identities = recipients match {
        case AllUsers => {
          if (req.isAdmin) {
            userCache.request(UserCacheMessages.GetAllUserIdsForAdmin(req)) {
              case UserCacheMessages.AllUserIds(ids) => {
                // Dole the actual sending out to each User Session
                // TODO: this does not scale well. We really ought to handle System Messages in some other way!
                ids.foreach { id => sessionManager ! UserSessionMessages.NewNotification(id, note) }
              }
            }
          } else {
            QLog.error("NotificationActor received an AllUsers message from someone who isn't an admin:\n" + msg)
          }
        }
        
        case AllMembers(spaceId) => {
          // TODO
        }
        
        case ExplicitRecipients(identityIds) => {
          // TODO
        }
      }
    }
  }
}

object NotificationActor {
  case class SendNotification(req:User, as:PublicIdentity, recipients:Recipients, note:Notification)
}