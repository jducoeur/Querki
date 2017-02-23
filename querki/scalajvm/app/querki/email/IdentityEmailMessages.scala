package querki.email

import querki.identity.IdentityId
import querki.notifications.Notification

object IdentityEmailMessages {
  sealed trait IdentityEmailMsg {
    def to:IdentityId
  }
  
  case class NotificationToEmail(note:Notification) extends IdentityEmailMsg {
    def to = note.toIdentity.get
  }
}
