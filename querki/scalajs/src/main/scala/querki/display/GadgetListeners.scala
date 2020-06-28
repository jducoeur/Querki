package querki.display

import org.scalajs.dom.html
import org.querki.gadgets.Gadget
import org.querki.jquery._

/**
 * This is similar to InputDependencies, but with a somewhat different focus. It is designed to enable one Gadget
 * to hook itself to another, by DOM id. Requires cooperation of both sides, at least in the current design.
 */
trait GadgetListeners {
  /**
   * Registers the given Gadget based on its DOM id. This should be called *after* the Gadget is created.
   */
  def register[T <: html.Element](gadget: Gadget[T]): Unit

  /**
   * Registers a listener callback, that will be called when the named Gadget is registered.
   *
   * The callback may be called in-thread if the named Gadget is already registered.
   */
  def listenFor(id: String)(handler: Gadget[_] => Unit): Unit

  /**
   * Clears out all of the maps in here.
   */
  def clear(): Unit
}

class GadgetListenersImpl extends GadgetListeners {
  type Listener = Gadget[_] => Unit

  var registeredGadgets: Map[String, Gadget[_]] = Map.empty

  var waitingListeners: Map[String, Set[Listener]] = Map.empty

  def register[T <: html.Element](gadget: Gadget[T]): Unit = {
    val elem: html.Element = gadget.elem
    $(elem).prop("id").toOption.map { idAny =>
      val id = idAny.asInstanceOf[String]
      registeredGadgets += (id -> gadget)

      // If anybody's waiting for this, fire them off:
      waitingListeners.get(id).map { listeners: Set[Listener] =>
        listeners.map(_(gadget))
        waitingListeners -= id
      }
    }
  }

  def listenFor(id: String)(handler: Gadget[_] => Unit): Unit = {
    registeredGadgets.get(id) match {
      // We already have it, so fire the listener now:
      case Some(gadget) => handler(gadget)
      // Wait for it to show up later:
      case None => {
        val listeners = waitingListeners.get(id).getOrElse(Set.empty)
        waitingListeners += (id -> (listeners + handler))
      }
    }
  }

  def clear(): Unit = {
    registeredGadgets = Map.empty
    waitingListeners = Map.empty
  }
}