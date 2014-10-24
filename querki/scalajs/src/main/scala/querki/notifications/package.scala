package querki

import rx._

import querki.globals._

package object notifications {
  trait Notifications extends EcologyInterface {
    /**
     * Fetch the current number of Notifications. Note that this is a reactive value, and may
     * change.
     */
    def numNotifications:Rx[Int]
  }
}
