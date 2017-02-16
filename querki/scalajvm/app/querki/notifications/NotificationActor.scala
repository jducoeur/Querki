package querki.notifications

import akka.actor._

import org.querki.requester._

import querki.ecology._
import querki.identity.{IdentityCacheMessages, PublicIdentity, User, UserCacheMessages, UserLevel}
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
  import UserNotificationActor._
  
  lazy val AdminOps = interface[querki.admin.AdminOps]
  lazy val IdentityAccess = interface[querki.identity.IdentityAccess]
  lazy val Notifications = interface[Notifications]
  
  lazy val userCache = IdentityAccess.userCache
  
  def receive = {
    case msg @ SendNotification(req, recipients, note) => {
      
      val identities = recipients match {
        case AllUsers => {
          if (req.isAdmin) {
            loopback(AdminOps.getAllUserIds(req)) foreach { ids =>
              // Dole the actual sending out to each User Session
              // TODO: this does not scale well. We really ought to handle System Messages in some other way!
              // Specifically, we should be streaming those UserIds in, and sending out the messages from that
              // stream. This may be the straw that breaks the camel's back and forces us to begin to transition
              // to Slick.
              ids.foreach { id => Notifications.userNotifications ! NewNotification(id, note) }
            }
          } else {
            QLog.error("NotificationActor received an AllUsers message from someone who isn't an admin:\n" + msg)
          }
        }
        
        case AllMembers(spaceId) => {
          // TODO
        }
        
        case ExplicitRecipients(identityIds) => {
          // The actual UserIds will be filled in by the router:
          IdentityAccess.routeToUsers(identityIds, Notifications.userNotifications, NewNotification(User.Anonymous.id, note))
        }
      }
    }
  }
}

object NotificationActor {
  case class SendNotification(req:User, recipients:Recipients, note:Notification)
}