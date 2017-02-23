package querki.email

import scala.concurrent.Future

import querki.identity.FullIdentity
import querki.notifications.Notification

/**
 * This is a companion trait to Notifier. If a Notifier has an EmailNotifier, then Notifications of that
 * sort will be passed on to it for possible mailing.
 * 
 * This lives in a funny grey zone between Notifications and Email.
 */
trait EmailNotifier {
  /**
   * Decides whether this specific Notification should get sent out, based on existing Unsubs.
   */
  def shouldSendEmail(note:Notification):Boolean
  
  /**
   * Turn this Notification into an EmailMsg, ready to send out.
   */
  def toEmail(note:Notification, recipient:FullIdentity):Future[EmailMsg]
}