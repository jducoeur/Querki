package querki

import models.PType

package object uservalues {
  /**
   * Handle for a PType that descends from UserValueType. Lets you get at the bits.
   */
  trait TUserValue {
    /**
     * The PType that we are presenting to the user.
     */
    def userType:PType[_]
  }
}