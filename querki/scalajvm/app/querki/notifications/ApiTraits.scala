package querki.notifications

import scala.concurrent.Future

import models.{OID, Wikitext}

import querki.email.EmailNotifier
import querki.identity.{IdentityId, UserId, UserLevel}
import querki.time.DateTime
import querki.values.QLContext

/**
 * Identifies a particular Notifier. Used so that we know who handles a given Notification.
 */
case class NotifierId(ecotId:Short, notificationType:Short) {
  override def toString = ((ecotId << 16) + notificationType).toString
}
object NotifierId {
  def apply(str:String):NotifierId = {
    val raw = str.toInt
    val ecotId = (raw >> 16).toShort
    val notificationType = (raw & 0xffff).toShort
    NotifierId(ecotId, notificationType)
  }
}
  
/**
 * The basic information about a given User.
 */
case object LoadInfo
case class UserNotificationInfo(userId:UserId, lastNoteChecked:Int)

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
  def summarizeNew(context:QLContext, notes:Seq[Notification]):Future[SummarizedNotifications]
  
  /**
   * Says how to display this Notification.
   */
  def render(context:QLContext, note:Notification):Future[RenderedNotification]
  
  /**
   * Iff this sort of Notification can be emailed, this is a pointer saying how.
   */
  def emailNotifier:Option[EmailNotifier]
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
case class Notification(
  id:NotificationId, 
  sender:IdentityId, 
  toIdentity:Option[IdentityId], 
  notifier:NotifierId, 
  sentTime:DateTime, 
  spaceId:Option[OID], 
  thingId:Option[OID], 
  payload:NotificationPayload, 
  isRead:Boolean = false, 
  isDeleted:Boolean = false)

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
