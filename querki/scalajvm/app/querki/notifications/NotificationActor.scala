package querki.notifications

import akka.actor._

import org.querki.requester._

import querki.ecology._
import querki.email.IdentityEmailMessages._
import querki.identity.{IdentityCacheMessages, IdentityId, PublicIdentity, User, UserCacheMessages, UserLevel}
import querki.util._

/**
 * For the moment, this is a singleton that manages Notifications. In principle this is wrong, but in practice
 * the amount of work per notification is fairly small for now.
 * 
 * TODO: eventually, restructure this into a pool of workers, or even just turn it into a function in the
 * NotificationEcot -- it's not at all clear that having this as its own Actor gains us anything at all.
 */
private[notifications] class NotificationActor(val ecology:Ecology) extends Actor with EcologyMember with Requester {
  import NotificationActor._
  import UserNotificationActor._
  
  lazy val AdminOps = interface[querki.admin.AdminOps]
  lazy val Email = interface[querki.email.Email]
  lazy val IdentityAccess = interface[querki.identity.IdentityAccess]
  lazy val Notifications = interface[Notifications]
  
  lazy val userCache = IdentityAccess.userCache
  
  def forwardToEmail(note:Notification, ids:Seq[IdentityId]):Unit = {
    val notifier = Notifications.notifierFor(note)
    if (notifier.emailNotifier.isDefined) {
      ids foreach { identityId =>
        val targetedNote = note.copy(toIdentity = Some(identityId))
        Email.identityEmail ! NotificationToEmail(targetedNote)
      }
    }
  }
  
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
              // TODO: do something similar to forwardToEmail, but we need the IdentityIds for each, and really
              // ought to be streaming them in. This *begs* for Slick.
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
          forwardToEmail(note, identityIds)
        }
      }
    }
  }
}

object NotificationActor {
  case class SendNotification(req:User, recipients:Recipients, note:Notification)
}