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
     * The URL path to get to the system images.
     */
    def imagePath:String
  }
}