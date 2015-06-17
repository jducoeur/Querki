package querki.display.rx

import org.scalajs.dom
import rx._

import querki.display.Gadget

/**
 * A wrapper around Gadgets, to make them a bit easier to use.
 * 
 * @author jducoeur
 */
class RxGadget[G <: Gadget[_]] {
  /**
   * The underlying Gadget that we're tracking here. If you want to react to the
   * Gadget's creation, or be safe when you aren't sure about creation order, observe this.
   */
  val gadgetOpt = Var[Option[G]](None)
  
  /**
   * Returns the underlying Gadget. Use with care: this will throw if the Gadget hasn't been
   * created yet!
   */
  def gadget = gadgetOpt().get
  
  /**
   * Set this to the actual Gadget when it's created. We only expect this to be called once
   * per RxGadget, although nothing currently enforces that.
   */
  def apply(g:G):G = {
    gadgetOpt() = Some(g)
    g
  }
}

object RxGadget {
  implicit def rx2Gadget[G <: Gadget[_]](rx:RxGadget[G]):G = rx.gadget
  
  def apply[G <: Gadget[_]] = new RxGadget[G]
  def of[T <: dom.Element] = new RxGadget[Gadget[T]]
}