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
  }
}
