package querki

import play.api.mvc.RequestHeader

import querki.ecology._
import querki.identity.User

package object session {

  /**
   * Provides access to the UserSessions.
   */
  trait Session extends EcologyInterface {
  }
  
}