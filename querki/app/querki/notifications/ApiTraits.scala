package querki.notifications

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
}