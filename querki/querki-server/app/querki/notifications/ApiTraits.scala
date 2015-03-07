package querki.notifications

import models.{OID, Wikitext}

import querki.identity.{IdentityId, UserId, UserLevel}
import querki.time.DateTime
import querki.values.QLContext

/**
 * Identifies a particular Notifier. Used so that we know who handles a given Notification.
 */
case class NotifierId(ecotId:Short, notificationType:Short)
  
/**
 * The basic information about a given User.
 * 
 * TODO: this is a bad smell. It is here because NotificationPersister is loading it. It probably
 * shouldn't be doing so.
 */
case object LoadInfo
case class UserInfo(userId:UserId, version:Int, lastNoteChecked:Int)

/**
 * Signal to update the last-checked Notification for this user.
 */
case class UpdateLastChecked(lastChecked:Int)

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
  
  /**
   * Says how to display this Notification.
   */
  def render(context:QLContext, note:Notification):RenderedNotification
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
 * The Notifications that are currently unread for this User.
 */
case class CurrentNotifications(notes:Seq[Notification])

/**
 * A single Notification in the system.
 */
case class Notification(id:NotificationId, sender:IdentityId, toIdentity:Option[IdentityId], notifier:NotifierId, sentTime:DateTime, spaceId:Option[OID], thingId:Option[OID], payload:NotificationPayload, 
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
