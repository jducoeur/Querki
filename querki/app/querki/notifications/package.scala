package querki

import models.Thing.PropMap

import querki.ecology._
import querki.identity.{IdentityId, PublicIdentity, User}

package object notifications {
  /**
   * The ID used for each Notification. This implementation is highly subject to change. 
   */
  type NotificationId = String
  
  val EmptyNotificationId = ""
  
  /**
   * How we represent the guts of a Notification. This may continue to be exactly a PropMap, but
   * it's helpful to at least mark it as a separate Type.
   */
  type NotificationPayload = PropMap
  
  /**
   * All Notifiers should register themselves in here. This can and usually should happen during
   * postInit(), to avoid pointless initialization dependencies.
   */
  trait NotifierRegistry extends EcologyInterface {
    /**
     * Registers the given Notifier, so that it can be used to send and interpret Notifications.
     */
    def register(notifier:Notifier)
    
    /**
     * Remove this notifier. Should be called during term().
     */
    def unregister(notifier:Notifier)
  }
  
  /**
   * The API for sending Notifications of any sort.
   */
  trait Notifications extends EcologyInterface {
    /**
     * Send this Notification to all of the specified Recipients. Fire and Forget!
     */
    def send(req:User, as:PublicIdentity, recipients:Recipients, note:Notification)
  }
}
