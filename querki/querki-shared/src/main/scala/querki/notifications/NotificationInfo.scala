package querki.notifications

import models.Wikitext

import querki.data.IdentityInfo
import querki.time.Common.Timestamp

/**
 * Common types for Notifications.
 *
 * This is a bit of a hack; it comes from the fact that you can't have multiple package objects
 * in the same package, which makes refactoring into shared a bit nasty.
 */
object Common {
  /**
   * The ID used for each Notification. Note that this is scoped by the receiver.
   */
  type NotificationId = Int
  
  val EmptyNotificationId = -1
}
import Common._
  
/**
 * How to display a single Notification.
 */
case class RenderedNotification(headline:Wikitext, content:Wikitext)

case class NotificationInfo(
  id:NotificationId,
  sender:IdentityInfo,
  spaceId:String,
  thingId:String,
  sentTime:Timestamp,
  rendered:RenderedNotification,
  isRead:Boolean,
  isDeleted:Boolean
)
