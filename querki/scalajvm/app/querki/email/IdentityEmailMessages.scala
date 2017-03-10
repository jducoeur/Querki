package querki.email

import models._
import querki.identity.IdentityId
import querki.notifications.Notification

object IdentityEmailMessages {
  sealed trait IdentityEmailMsg {
    def to:IdentityId
  }
  
  case class NotificationToEmail(note:Notification) extends IdentityEmailMsg {
    def to = note.toIdentity.get
  }
  
  case class DoUnsubscribe(to:IdentityId, notifier:String, unsubId:OID, context:Option[String]) extends IdentityEmailMsg
  case class Unsubscribed(message:Wikitext)
}
