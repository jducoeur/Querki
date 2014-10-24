package querki.display

import org.scalajs.dom

import querki.globals._

package object input {
  trait InputGadgets extends EcologyInterface {
    /**
     * Given a root element (usually one that has been newly created from server-sent, non-Scalatags code),
     * look for any InputGadgets that should be created in it, based on their class.
     * 
     * IMPORTANT: these InputGadgets are (intentionally) not immediately hooked! Their hook() method will
     * be called when they actually get added to the DOM and shown, since some controls depend on that.
     */
    def createInputGadgets(root:dom.Element):Unit
    
    /**
     * Hook all Gadgets that have been created but not yet hooked.
     * 
     * IMPORTANT: you *MUST* call this any time you add Page content that may potentially contain
     * InputGadgets!
     */
    def hookPendingGadgets():Unit
    
    /**
     * Tells any InputGadgets that are currently being edited to save their current values, and returns
     * a Future that will be fulfilled once that is true. (Which may be immediately.)
     */
    def afterAllSaved:Future[Unit]
  }
}
