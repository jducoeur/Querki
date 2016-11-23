package querki.display.rx

import rx._

import querki.display.Gadget

/**
 * A handy typeclass for asking whether a reactive Gadget is "empty" or not. The
 * meaning of "empty" depends on the Gadget, obviously.
 * 
 * Use this by importing RxEmptyable._, and then you can use .rxEmpty on the
 * appropriate Gadgets -- that returns an Rx[Boolean], suitable for using on, eg,
 * the disabled attribute.
 */
trait RxEmptyable[T] {
  def rxEmpty(elem:T):Rx[Boolean]
}

object RxEmptyable {
  implicit object RxEmptyableTextArea extends RxEmptyable[RxTextArea] {
    def rxEmpty(elem:RxTextArea) = Rx { elem.textOpt().isEmpty }
  }
  implicit def RxEmptyableInput[T <: RxInput] = new RxEmptyable[T] {
    def rxEmpty(elem:T) = Rx { elem.textOpt().isEmpty }
  }
  implicit def RxEmptyableGadgetRef[G <: Gadget[_] : RxEmptyable] = new RxEmptyable[GadgetRef[G]] {
    def rxEmpty(ref:GadgetRef[G]):Rx[Boolean] = {
      ref.map { g =>
        implicitly[RxEmptyable[G]].rxEmpty(g)
      }.getOrElse(Rx{true})
    }
  }
  
  /**
   * This adds a simple .rxEmpty method to any Gadget that has RxEmptyable defined.
   */
  implicit class RxEmptyableConv[T : RxEmptyable](elem:T) {
    def rxEmpty:Rx[Boolean] = implicitly[RxEmptyable[T]].rxEmpty(elem)
  }
}
