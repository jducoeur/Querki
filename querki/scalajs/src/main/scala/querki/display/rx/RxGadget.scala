package querki.display.rx

import org.scalajs.dom
import scalatags.JsDom.all._
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
  val opt = Var[Option[G]](None)
  
  /**
   * The safe way to run some code iff this Gadget actually exists.
   */
  def map[T](f:G => T):Option[T] = opt().map(f)
  
  def flatMap[T](f:G => Option[T]) = opt().flatMap(f)
  
  /**
   * Returns the underlying Gadget. Use with care: this will throw if the Gadget hasn't been
   * created yet!
   */
  def get = opt().get
  
  def isDefined = opt().isDefined
  def isEmpty = opt().isEmpty
  
  /**
   * Set this to the actual Gadget when it's created. We only expect this to be called once
   * per RxGadget, although nothing currently enforces that.
   */
  def <=(g:G):G = {
    opt() = Some(g)
    g
  }
  
  def <=[T <: dom.Element](tag:scalatags.JsDom.TypedTag[T]):G = {
    // TODO: this is evil and wrong. How can I fix up the type signatures to get G right,
    // without having to specify *both* G and T all the time?
    val g = Gadget(tag).asInstanceOf[G]
    <=(g)
  }
  
  /**
   * This defines a callback for when the Gadget actually gets defined. Note that this does *not*
   * mean that the underlying Element has been created!
   * 
   * This function is chainable.
   */
  def whenSet(f:G => Unit) = {
    Obs(opt) {
      map(f)
    }
    this
  }
}

object RxGadget {
  /**
   * Given an RxGadget, cast it to the actual underlying gadget.
   * 
   * IMPORTANT: this uses RxGadget.gadget under the hood, and should only be invoked if you are
   * confident that the gadget has been initialized!
   */
  implicit def rx2Gadget[G <: Gadget[_]](rx:RxGadget[G]):G = rx.get
  
  def apply[G <: Gadget[_]] = new RxGadget[G]
  def of[T <: dom.Element] = new RxGadget[Gadget[T]]
}