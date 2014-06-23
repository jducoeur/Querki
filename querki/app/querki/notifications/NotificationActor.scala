package querki.notifications

import akka.actor._

import querki.ecology._
import querki.identity.{PublicIdentity, User, UserLevel}
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
  
  def receive = {
    case msg @ SendNotification(req, as, recipients, note) => {
      // TEMP:
      QLog.spew(s"${as.name} sent $note to $recipients")
      val identities = recipients match {
        case AllUsers => {
          if (req.isAdmin) {
            // TODO
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