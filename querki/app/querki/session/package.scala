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
  }
  
}