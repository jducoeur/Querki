package querki

import scala.concurrent.Future

import akka.actor.{ActorRef, Props}

import anorm.SqlQuery

import models._

import querki.ecology._
import querki.identity.{IdentityId, PublicIdentity, User, UserId}
import querki.notifications.Common
import querki.spaces.SerializedProps
import querki.values.{RequestContext, SpaceState}

package object notifications {
  /**
   * The ID used for each Notification. Note that this is scoped by the receiver.
   */
  type NotificationId = Common.NotificationId
  
  val EmptyNotificationId = Common.EmptyNotificationId
  
  final val NotifyTag = "Notifications"
  
  /**
   * How we represent the guts of a Notification. Note that this is necessarily a *serialized*
   * PropMap, because we use it in cross-node messages.
   * 
   * IMPORTANT: there's an implication here that this can therefore only contain Properties in
   * System, since we are deserializing it without knowing the Space context.
   */
  type NotificationPayload = SerializedProps
  
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
     * The master router for UserNotifications -- all messages destined for a UserNotificationActor
     * should go through here.
     */
    def userNotifications:ActorRef
    
    /**
     * Fetch a registered Notifier.
     */
    def notifier(id:NotifierId):Notifier
    
    /**
     * Fetch the Notifier for this Notification.
     */
    def notifierFor(note:Notification):Notifier
    
    /**
     * Send this Notification to all of the specified Recipients. Fire and Forget!
     */
    def send(req:User, recipients:Recipients, note:Notification)
    
    /**
     * Figures out how to display the given Notification.
     */
    def render(rc:RequestContext, note:Notification):Future[RenderedNotification]
  }
  
  /**
   * API for actually working with the DB.
   * 
   * Note that this is partial -- for a lot of the utilities, see SpacePersistence.
   */
  trait NotificationPersistence extends EcologyInterface {
    def noteTable(id:UserId):String
    def UserSQL(userId:UserId, query:String, version:Int = 0):SqlQuery
    def loadUserInfo(userId:UserId):Option[UserNotificationInfo]
    def updateLastChecked(userId:UserId, lastChecked:Int):Unit
    def loadCurrent(userId:UserId):CurrentNotifications
    def createNotification(userId:UserId, note:Notification):Unit
    def notificationPersisterProps(userId:UserId):Props
  }
}
