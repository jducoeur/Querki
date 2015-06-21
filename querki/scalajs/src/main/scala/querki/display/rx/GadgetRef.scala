package querki.display.rx

import org.scalajs.dom
import scalatags.JsDom.all._
import rx._

import querki.display.{Gadget, TypedGadget}

/**
 * A wrapper around Gadgets, to make them a bit easier to use. 
 * 
 * Think of this as a reactive container into which you put a Gadget of a particular type, 
 * which other code can hang off of. This is how you build complex reactive structures, while
 * still keep the declarations of the Gadgets in the Scalatags hierarchy so that you can
 * see what goes where.
 * 
 * So you typically declare this as an empty holder, and then assign to it later in the
 * Scalatags, like this:
 * {{{
 * val myGadget = GadgetRef[MyGadget]
 * 
 * def funcUsingMyGadget = {
 *   myGadget.someFunction()
 * }
 * 
 * def funcUsingMyGadgetIfExists = {
 *   myGadget.map(_.someOtherFunction())
 * }
 * ...
 * val rendered =
 *   p(
 *     "Here's the gadget",
 *     myGadget <= new MyGadget(...)
 *   )
 * }}}
 * Note that the GadgetRef will implicitly unwrap to the underlying Gadget if the environment
 * calls for that. Use that with some care -- it will throw an exception if the Gadget hasn't
 * been set yet! If you aren't certain whether the Gadget has been set, use map() or flatMap()
 * instead, and it has Option-like semantics. 
 * 
 * You don't usually create this by hand -- use the methods in the companion object to make
 * GadgetRef and GadgetElementRef.
 * 
 * @author jducoeur
 */
class GadgetRef[G <: Gadget[_]] {
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
   * Set this to the actual Gadget when it's created. 
   * 
   * This is typically only called once per
   * GadgetRef, but that is specifically not enforced; it is occasionally appropriate to update
   * the gadget. Keep in mind that opt() will update when this happens! Note that setting this
   * does *not* cause the view to update; wrap the GadgetRef in an RxDiv if you want that to
   * happen.
   */
  def <=(g:G):G = {
    opt() = Some(g)
    g
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

/**
 * A variant of GadgetRef, which you should use when you're just dealing with a raw TypedTag, not
 * a Gadget per se.
 * 
 * All this really does is provide type-safety for this situation, without requiring that GadgetRef
 * itself know about T.
 * 
 * Create this using GadgetRef.of[].
 */
class GadgetElementRef[T <: dom.Element] extends GadgetRef[Gadget[T]] {
  /**
   * This is similar to GadgetRef <= operation, but works with a raw TypedTag and wraps it in a
   * Gadget. This is used when you declared it with GadgetRef.of[].
   */
  def <=(tag:scalatags.JsDom.TypedTag[T]):Gadget[T] = {
    val g = new TypedGadget(tag, { elem:T => })
    <=(g)
  }
}

object GadgetRef {
  /**
   * Given an GadgetRef, cast it to the actual underlying gadget.
   * 
   * IMPORTANT: this uses GadgetRef.gadget under the hood, and should only be invoked if you are
   * confident that the gadget has been initialized!
   */
  implicit def rx2Gadget[G <: Gadget[_]](rx:GadgetRef[G]):G = rx.get
  
  def apply[G <: Gadget[_]] = new GadgetRef[G]
  def of[T <: dom.Element] = new GadgetElementRef[T]
}