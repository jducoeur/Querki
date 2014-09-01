package querki

import querki.ecology._

import querki.identity.User
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
    def recordAccept(user:User, version:Int):User
  }
}