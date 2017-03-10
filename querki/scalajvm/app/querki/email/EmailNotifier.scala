package querki.email

import scala.concurrent.Future

import models.{OID, Wikitext}
import querki.identity.FullIdentity
import querki.notifications.{Notification, NotifierId}
import querki.persistence.UseKryo

import EmailFunctions._

/**
 * Marker trait signifying that this is a single "Unsubscribe" event. Deliberately opaque -- its
 * Notifier is responsible for actually processing it.
 */
trait UnsubEvent

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
  def shouldSendEmail(note:Notification, unsubs:List[UnsubEvent]):Boolean
  
  /**
   * Turn this Notification into an EmailMsg, ready to send out.
   */
  def toEmail(note:Notification, recipient:FullIdentity):Future[EmailMsg]

  /**
   * Gets the Unsubscription options the User has for the provided Unsubscribe link.
   */
  def unsubOptions(unsubInfo:UnsubInfo):Future[(Wikitext, Seq[UnsubOption])]
  
  /**
   * Based on the given info (which comes from unsubOptions()), generate a persistable
   * Event. Note that the returned structure *must* be Persistable!
   */
  def getUnsubEvent(unsubId:OID, context:Option[String]):(Wikitext, UnsubEvent with UseKryo)
}
