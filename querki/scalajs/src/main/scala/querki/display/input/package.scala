package querki.display

import org.scalajs.dom

import querki.globals._

package object input {
  trait InputGadgets extends EcologyInterface {
    /**
     * Given a root element (usually one that has been newly created from server-sent, non-Scalatags code),
     * look for any InputGadgets that should be hooked into it.
     */
    def hookGadgets(root:dom.Element):Unit
  }
}
