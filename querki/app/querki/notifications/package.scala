package querki

import akka.actor.Props

import anorm.SqlQuery

import models.Thing.PropMap

import querki.ecology._
import querki.identity.{IdentityId, PublicIdentity, User, UserId}

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
  
  /**
   * API for actually working with the DB.
   * 
   * Note that this is partial -- for a lot of the utilities, see SpacePersistence.
   */
  trait NotificationPersistence extends EcologyInterface {
    def noteTable(id:UserId):String
    def UserSQL(userId:UserId, query:String, version:Int = 0):SqlQuery
    def loadUserInfo(userId:UserId):Option[UserInfo]
    def notificationPersisterProps(userId:UserId):Props
  }
}
