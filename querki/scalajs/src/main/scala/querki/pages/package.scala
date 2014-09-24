package querki

import querki.globals._

package object pages {
  trait Pages extends EcologyInterface {
    /**
     * Given the information sent from the Client -- the ID of a Page class, and the
     * pickled information about that page -- build the page.
     */
    def constructPage(id:PageIDs.PageID, pickled:String):Page
  }
}
