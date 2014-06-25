package querki

import scala.concurrent.Future

import akka.actor.ActorRef

import querki.ecology._
import querki.identity.User

package object session {

  /**
   * Provides access to the UserSessions.
   */
  trait Session extends EcologyInterface {
    /**
     * The root of the UserSession hierarchy. Other Actors may use this directly to send messages to UserSessions.
     */
    def sessionManager:ActorRef
    
    /**
     * Asynchronously gets the current info to show this user.
     */
    def getSessionInfo(user:User):Future[UserSessionInfo]
    
    /**
     * Asynchronously fetches the notifications for this user, in reverse chronological order.
     * 
     * TODO: this is way too static. We *should* have a distinctly stream-oriented model for dealing
     * with this.
     */
    def getNotifications(user:User):Future[UserSessionMessages.RecentNotifications]
  }
  
}