package querki

import scala.concurrent.Future

import querki.ecology._
import querki.identity.User

package object session {

  /**
   * Provides access to the UserSessions.
   */
  trait Session extends EcologyInterface {
    /**
     * Asynchronously gets the current info to show this user.
     */
    def getSessionInfo(user:User):Future[UserSessionInfo]
  }
  
}