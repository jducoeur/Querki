package querki

import org.scalajs.dom

import querki.globals._

package object display {
  trait PageManager extends EcologyInterface {
    /**
     * Actually render the page, inside the given root.
     */
    def setRoot(root:dom.Element):Unit
    
    /**
     * Set the folder where images are kept.
     */
    def setImagePath(path:String):Unit
    
    /**
     * The URL path to get to the system images.
     */
    def imagePath:String
    
    /**
     * Actually show the page, passing in the pickled information. (Which may be empty string
     * if not relevant.)
     */
    def renderPage(pageID:querki.pages.PageIDs.PageID, pickled:String)
  }
}
