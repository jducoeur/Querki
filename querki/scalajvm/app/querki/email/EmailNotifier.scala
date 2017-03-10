package querki.email

import scala.concurrent.Future

import models.Wikitext
import querki.identity.FullIdentity
import querki.notifications.{Notification, NotifierId}

import EmailFunctions._

/**
 * This is a companion trait to Notifier. If a Notifier has an EmailNotifier, then Notifications of that
 * sort will be passed on to it for possible mailing.
 * 
 * This lives in a funny grey zone between Notifications and Email.
 */
trait EmailNotifier {
  /**
   * Usually the same field as from Notifier, but broken out for encapsulation purposes.
   */
  def id:NotifierId
  
  /**
   * Decides whether this specific Notification should get sent out, based on existing Unsubs.
   */
  def shouldSendEmail(note:Notification):Boolean
  
  /**
   * Turn this Notification into an EmailMsg, ready to send out.
   */
  def toEmail(note:Notification, recipient:FullIdentity):Future[EmailMsg]

  /**
   * Gets the Unsubscription options the User has for the provided Unsubscribe link.
   */
  def unsubOptions(unsubInfo:UnsubInfo):Future[(Wikitext, Seq[UnsubOption])]
}
