package org.querki.gadgets.reactive

import rx._
import org.querki.gadgets.core.{Gadget, GadgetRef}

/**
 * A handy typeclass for asking whether a reactive Gadget is "empty" or not. The
 * meaning of "empty" depends on the Gadget, obviously.
 * 
 * Use this by importing RxEmptyable._, and then you can use .rxEmpty on the
 * appropriate Gadgets -- that returns an Rx[Boolean], suitable for using on, eg,
 * the disabled attribute.
 */
trait RxEmptyable[T] {
  def rxEmpty(elem:T)(implicit ctx:Ctx.Owner):Rx[Boolean]
}

trait EmptyableCoreImplicits {
  /**
   * High-level version of .rxEmpty for GadgetRef -- basically, if the contained Gadget is itself
   * RxEmptyable, then the GadgetRef is as well.
   */
  implicit def RxEmptyableGadgetRef[G <: Gadget[_] : RxEmptyable]: RxEmptyable[GadgetRef[G]] = new RxEmptyable[GadgetRef[G]] {
    def rxEmpty(ref:GadgetRef[G])(implicit ctx:Ctx.Owner):Rx[Boolean] = {
      ref.flatMapRx { g =>
        implicitly[RxEmptyable[G]].rxEmpty(g)
      }.map(_.getOrElse(true))
    }
  }
  
  /**
   * This adds a simple .rxEmpty method to any Gadget that has RxEmptyable defined.
   */
  implicit class RxEmptyableConv[T : RxEmptyable](elem:T) {
    def rxEmpty(implicit ctx:Ctx.Owner):Rx[Boolean] = implicitly[RxEmptyable[T]].rxEmpty(elem)
  }
}
