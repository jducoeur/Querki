package querki

import akka.actor.Props

import anorm.SqlQuery

import models.Thing.PropMap

import querki.ecology._
import querki.identity.{IdentityId, PublicIdentity, User, UserId}
import querki.notifications.Common
import querki.values.RequestContext

package object notifications {
  /**
   * The ID used for each Notification. Note that this is scoped by the receiver.
   */
  type NotificationId = Common.NotificationId
  
  val EmptyNotificationId = Common.EmptyNotificationId
  
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
    def send(req:User, recipients:Recipients, note:Notification)
    
    /**
     * Figures out how to display the given Notification.
     */
    def render(rc:RequestContext, note:Notification):RenderedNotification
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
    def updateLastChecked(userId:UserId, lastChecked:Int):Unit
    def loadCurrent(userId:UserId):CurrentNotifications
    def createNotification(userId:UserId, note:Notification):Unit
    def notificationPersisterProps(userId:UserId):Props
  }
}
