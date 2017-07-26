package querki.display

import org.scalajs.dom
import org.querki.jquery._
import org.querki.gadgets._
import org.querki.gadgets.core.GadgetLookup

import querki.globals._

/**
 * This is a gadget that needs to be "hooked" -- it requires a call after its *container* is created.
 * This is most often relevant for gadgets that might be created from externally-loaded HTML, which
 * need to add behavior once everything is set up.
 * 
 * TODO: This doesn't really belong under display.input. Lift the hook stuff out of InputGadgets into
 * Gadgets. 
 * 
 * @author jducoeur
 */
abstract class HookedGadget[T <: dom.html.Element](e:Ecology) extends Gadget[T] with EcologyMember {
  implicit val ecology = e
  
  lazy val GadgetsInternal = interface[GadgetsInternal]
  
  /**
   * Hook whatever events are appropriate for this Gadget.
   */
  protected def hook():Unit
  
  /**
   * Called by InputGadgets when it is time to prepare this Gadget for the world.
   */
  def prep() = {
    // Don't hook templates! That causes nothing but havoc.
    // TODO: inputTemplate should simply go away ASAP -- it's an old approach to adding new list items.
    if (!$(elem).hasClass("inputTemplate")) {
      GadgetLookup.annotateGadget(this)
      hook()
    }
  }
  
  // Register ourselves, so that we get hooked. Note that hooking needs to happen *after* onCreate,
  // since some libraries operate on the context we are found in:
  GadgetsInternal.gadgetCreated(this)
}
