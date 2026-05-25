package org.querki.gadgets.core

import org.scalajs.dom.html.Element
import scalatags.JsDom.all._
import rx._
import org.querki.jquery._

/**
 * A reactive wrapper around Gadgets, to make them a bit easier to use.
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
 * TODO: is there any way to make this sensitive to the actual element type of the Gadget, without
 * forcing us to add another type parameter everywhere?
 *
 * TODO: this is annoyingly dependent on jQuery. Add the necessary bits to sQuery, so we can
 * remove the jQuery dependencies.
 *
 * @author jducoeur
 */
class GadgetRef[G <: Gadget[_]] extends Gadget[Element] {

  def doRender() = ???

  /**
   * The actual renderer for this reference.
   *
   * If this reference has not yet been set, it will create and return an empty span.
   *
   * TODO: this is type-anonymous -- it simply expects to return an Element. Can we do better?
   */
  override def createFrag = {
    opt.now.map(g => g.asInstanceOf[Gadget[Element]].render).getOrElse(span().render).asInstanceOf[Element]
  }

  /**
   * The underlying Gadget that we're tracking here. If you want to react to the
   * Gadget's creation, or be safe when you aren't sure about creation order, observe this.
   */
  val opt = Var[Option[G]](None)

  /**
   * The safe way to run some code iff this Gadget actually exists. This runs *immediately*, and does
   * not produce an Rx.
   */
  def mapNow[T](f: G => T): Option[T] = opt.now.map(f)

  /**
   * TODO: the semantics of flatMapNow() are, at best, questionable. This is trying to do too much, and
   * should be renamed or scrapped.
   */
  def flatMapNow[T](f: G => Option[T]) = opt.now.flatMap(f)
  def foreachNow(f: G => Unit): Unit = opt.now.foreach(f)

  /**
   * Shorthand for this extremely common case.
   */
  def mapNowOrElse[T](
    f: G => T,
    default: T
  ): T = mapNow(f).getOrElse(default)

  /**
   * Convenience wrapper for getting at the underlying element, if it's been set up yet.
   */
  def mapElemNow[T](f: Element => T): Option[T] = flatMapNow(_.asInstanceOf[Gadget[Element]].elemOpt.map(f))

  def foreachElemNow[T](f: Element => T): Unit = opt.now.foreach(_.asInstanceOf[Gadget[Element]].elemOpt.foreach(f))

  /**
   * This maps the given function over the underlying Gadget, when it exists. This
   * returns a proper Rx.
   */
  def mapRx[T](f: G => T)(implicit ctx: Ctx.Owner): Rx[Option[T]] = opt.map(_.map(f))

  /**
   * Produces Rx[Some[T]] if the Gadget is ready, or Rx[None] if not.
   */
  def flatMapRx[T](f: G => Rx[T])(implicit ctx: Ctx.Owner): Rx[Option[T]] = opt.flatMap { gOpt =>
    gOpt.map { g => f(g).map(Some(_)) }.getOrElse(Var[Option[T]](None))
  }

  /**
   * Iff this Gadget is ready, produces the result of the expression; otherwise, produces the default.
   */
  def mapRxOrElse[T](
    f: G => T,
    default: T
  )(implicit
    ctx: Ctx.Owner
  ): Rx[T] = mapRx(f).map(_.getOrElse(default))

  /**
   * The true boilerplate-killer. Takes an Rx to flatMap over -- that is, a function on the underlying
   * Gadget that produces another Rx. *If* the Gadget is ready, it then applies function f to that
   * Rx; otherwise, it produces the default.
   */
  def flatMapRxOrElse[T, U](
    rx: G => Rx[T]
  )(
    f: T => U,
    default: U
  )(implicit
    ctx: Ctx.Owner
  ): Rx[U] = {
    flatMapRx(rx).map(_.map(f).getOrElse(default))
  }

  /**
   * Just delegate the insertion signal to the underlying Gadget, if there is one.
   */
  override def onInserted() = foreachNow(_.onInserted())

  /**
   * Returns the underlying Gadget. Use with care: this will throw if the Gadget hasn't been
   * created yet!
   */
  def get = opt.now.get

  def isDefined = opt.now.isDefined
  def isEmpty = opt.now.isEmpty

  /**
   * Reassigns the contents of this reference, rendering the new Gadget and putting it into place if needed.
   * This will only be called if something actually changes.
   *
   * This is usually called via the <= or <~ operators.
   *
   * Note that this requires a
   *
   * TODO: there is a grungy asInstanceOf in here. Is there any way to pick up the actual type of the
   * underlying Gadget without polluting the type signature of this call in such a way that you have to
   * redundantly state it every time?
   */
  def reassign[N : GadgetNotifier](
    g: G,
    retainPrevious: Boolean
  ): this.type = {
    if (elemOpt.isDefined && g.elemOpt.isDefined && elemOpt.get == g.elemOpt.get) {
      // Reassigning the same value is a no-op
    } else {
      val prevElemOpt = elemOpt

      // If we've been through rendering, render and insert the new one:
      parentOpt.map { parent =>
        val r = g.rendered.asInstanceOf[Element]
        $(r).insertBefore(elem)
        elemOptRx() = Some(r)
        g.onInserted()
      }

      // Detach/remove the previous element from the DOM, if it is there:
      prevElemOpt.map { e =>
        if (retainPrevious)
          $(e).detach()
        else
          $(e).remove()
      }

      opt() = Some(g)

      implicitly[GadgetNotifier[N]].layoutChanged(this)
    }
    this
  }

  /**
   * Set this to the actual Gadget when it's created.
   *
   * If this reference was already rendered, this will render the new gadget and reparent it where the
   * reference lives.
   */
  def <=[N : GadgetNotifier](g: G) = reassign(g, false)

  /**
   * The same as <=, but doesn't remove the old value from the DOM. Use this if you expect to reuse the
   * Gadgets that you are assigning. (Typically when this reference is somehow modal, and you don't want
   * to regenerate the Gadgets every time because they are expensive.)
   */
  def <~[N : GadgetNotifier](g: G) = reassign(g, true)

  /**
   * This defines a callback for when the Gadget actually gets defined. Note that this does *not*
   * mean that the underlying Element has been created! If you care about the underlying DOM element,
   * use whenRendered() instead.
   *
   * This function is chainable.
   *
   * TBD: is this *ever* the right answer? We might consider deprecating this as a public entry point,
   * since you usually want whenRendered() instead.
   */
  def whenSet(f: G => Unit)(implicit ctx: Ctx.Owner): this.type = {
    opt.trigger {
      mapNow(f)
      ()
    }
    this
  }

  /**
   * Invoke the given function when this Gadget is actually rendered. At that point, the Gadget's elem
   * has been set, so you can operate on it. This is sometimes the right place to add, eg, event hooks.
   */
  def whenRendered(f: G => Unit)(implicit ctx: Ctx.Owner): this.type = {
    whenSet { g =>
      if (g.elemOpt.isDefined)
        f(g)
      else {
        g.elemOptRx.trigger {
          if (g.elemOpt.isDefined)
            f(g)
        }
      }
    }
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
class GadgetElementRef[T <: Element] extends GadgetRef[Gadget[T]] {

  /**
   * This is similar to GadgetRef <= operation, but works with a raw TypedTag and wraps it in a
   * Gadget. This is used when you declared it with GadgetRef.of[].
   */
  def <=[N : GadgetNotifier](tag: scalatags.JsDom.TypedTag[T]) = reassign(new TypedGadget(tag), false)
  def <~[N : GadgetNotifier](tag: scalatags.JsDom.TypedTag[T]) = reassign(new TypedGadget(tag), true)
}

object GadgetRef {

  /**
   * The usual way to create a GadgetRef for a particular type of Gadget.
   */
  def apply[G <: Gadget[_]] = new GadgetRef[G]

  /**
   * This allows you to create a GadgetRef that will work with an underlying TypedTag.
   */
  def of[T <: Element] = new GadgetElementRef[T]
}
