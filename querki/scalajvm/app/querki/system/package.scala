package querki

import scala.concurrent.Future

import querki.ecology._

import querki.identity.User
import querki.session.UserFunctions.TOSState
import querki.values.SpaceState

package object system {

  trait System extends EcologyInterface {
    /**
     * This is the Officially Correct Way to get at the System Space. If you can get to it this way, do
     * so instead of using the old backdoor through SystemSpace.State, which will eventually go away.
     */
    def State:SpaceState
  }
  
  trait TermsOfService extends EcologyInterface {
    def checkTOS(user:User):TOSState
    def currentTOS:TOSVersion
    def recordAccept(user:User, version:Int):Future[User]
  }
}
