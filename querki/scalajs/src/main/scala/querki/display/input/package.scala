package querki.display

import org.scalajs.dom

import querki.globals._

package object input {
  trait InputGadgets extends EcologyInterface {
    /**
     * Given a root element (usually one that has been newly created from server-sent, non-Scalatags code),
     * look for any InputGadgets that should be hooked into it.
     */
    def hookRawGadgets(root:dom.Element):Unit
    
    /**
     * Hook all Gadgets that have been created but not yet hooked. This usually should be done as soon as
     * the page is finished loading.
     */
    def hookPendingGadgets():Unit
    
    /**
     * Each InputGadget should register itself here, to ensure that it gets hooked.
     */
    def gadgetCreated(gadget:InputGadget[_]):Unit
    
    /**
     * Record that this Gadget has begun to be edited, and is not yet saved. Use this for complex Gadgets
     * that don't simply save immediately on every change, so that we can force-save when needed.
     */
    def startingEdits(gadget:InputGadget[_]):Unit
    
    /**
     * The pair to startingEdits(), which should be called when save is complete.
     */
    def saveComplete(gadget:InputGadget[_]):Unit
    
    /**
     * Tells any InputGadgets that are currently being edited to save their current values, and returns
     * a Future that will be fulfilled once that is true. (Which may be immediately.)
     */
    def afterAllSaved:Future[Unit]
  }
}
