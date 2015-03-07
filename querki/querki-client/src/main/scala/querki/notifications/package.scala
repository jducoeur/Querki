package querki

import rx._

import querki.globals._

import querki.comm.URL

package object notifications {
  
  trait Notifications extends EcologyInterface {
    
    /**
     * Fetch the URL of the Notifications Page.
     */
    def notificationPageUrl:URL
  
    /**
     * Fetch the current number of Notifications. Note that this is a reactive value, and may
     * change.
     */
    def numNotifications:Rx[Int]
    
    /**
     * Check the number of notifications on the server, and update the NotifierGadget if need be.
     */
    def checkNotifications():Unit
  }
}
