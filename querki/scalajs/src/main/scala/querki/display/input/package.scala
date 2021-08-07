package querki.display

import org.scalajs.dom

import querki.globals._

package object input {

  trait InputGadgets extends EcologyInterface {

    /**
     * Tells any InputGadgets that are currently being edited to save their current values, and returns
     * a Future that will be fulfilled once that is true. (Which may be immediately.)
     */
    def afterAllSaved: Future[Unit]
  }
}
