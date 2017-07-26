package querki.display.rx

import rx._

import org.querki.gadgets._

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
  implicit def RxEmptyableText[T <: RxTextBase[_]] = new RxEmptyable[T] {
    def rxEmpty(elem:T) = Rx { elem.textOpt().isEmpty }
  }
  
  implicit val RxEmptyableRadio = new RxEmptyable[RxRadio] {
    def rxEmpty(elem:RxRadio) = Rx { elem.selectedOption().isEmpty }
  }
  
  /**
   * High-level version of .rxEmpty for GadgetRef -- basically, if the contained Gadget is itself
   * RxEmptyable, then the GadgetRef is as well.
   */
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
