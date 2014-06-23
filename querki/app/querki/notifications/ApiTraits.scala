package querki.notifications

import models.{OID, Wikitext}

import querki.identity.{IdentityId, UserLevel}
import querki.time.DateTime
import querki.values.QLContext

/**
 * Identifies a particular Notifier. Used so that we know who handles a given Notification.
 */
case class NotifierId(ecotId:Short, notificationType:Short)

/**
 * A plugin that is published by some Ecot, which encapsulates the handling for one particular kind of Notification.
 */
trait Notifier {
  /**
   * Which one am I?
   */
  def id:NotifierId
  
  /**
   * What level should these Notifications be summarized at?
   */
  def summarizeAt:SummarizeAt.SummarizeAt
  
  /**
   * Takes a bunch of unread Notifications, and returns a summary of them.
   */
  def summarizeNew(context:QLContext, notes:Seq[Notification]):SummarizedNotifications
}

/**
 * Represents the way a given Notifier wants its Notifications summarized for the main display.
 */
object SummarizeAt {
  type SummarizeAt = Int
  
  val None = 0
  val Thing = 1
  val Space = 2
}

/**
 * When we summarize a bunch of Notifications, this is what we get back. It's approximately the usual
 * Subject/Body pair, but I don't want to bias the semantics.
 */
case class SummarizedNotifications(headline:Wikitext, content:Wikitext, notes:Seq[Notification])

/**
 * A single Notification in the system.
 */
case class Notification(id:NotificationId, sender:IdentityId, notifier:NotifierId, sentTime:DateTime, spaceId:Option[OID], thingId:Option[OID], payload:NotificationPayload, 
    isRead:Boolean = false, isDeleted:Boolean = false)

/**
 * Represents a person or persons who a Notification can be sent to. 
 */
sealed trait Recipients

/**
 * This Notification is to be sent to all users in Querki. Requires admin rights.
 */
case object AllUsers extends Recipients

/**
 * This Notification should go to all Members of the specified Space.
 */
case class AllMembers(spaceId:OID) extends Recipients

/**
 * This Notification should be sent to some specific people.
 */
case class ExplicitRecipients(identities:Seq[IdentityId]) extends Recipients
