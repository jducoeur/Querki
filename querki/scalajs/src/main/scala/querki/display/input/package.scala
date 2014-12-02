package querki.display

import org.scalajs.dom

import querki.globals._

package object input {
  trait InputGadgets extends EcologyInterface {
    /**
     * Hook all Gadgets that have been created but not yet hooked.
     * 
     * IMPORTANT: you *MUST* call this any time you add Page content that may potentially contain
     * InputGadgets! But this must be called *AFTER* that content is fully added to the DOM tree,
     * and shown!
     */
    def hookPendingGadgets():Unit
    
    /**
     * Tells any InputGadgets that are currently being edited to save their current values, and returns
     * a Future that will be fulfilled once that is true. (Which may be immediately.)
     */
    def afterAllSaved:Future[Unit]
  }
}
