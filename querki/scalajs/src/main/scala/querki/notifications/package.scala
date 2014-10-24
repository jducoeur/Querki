package querki

import querki.globals._

package object notifications {
  trait Notifications extends EcologyInterface {
    /**
     * Fetch the current number of Notifications.
     * 
     * TODO: this should really be fetching a reactive value! This may finally be a good
     * use for Scala.Rx in my code, so that it can return the current value immediately, but
     * also initiate a fetch and update the value if it turns out to have updated.
     */
    def numNotifications:Int
  }
}
