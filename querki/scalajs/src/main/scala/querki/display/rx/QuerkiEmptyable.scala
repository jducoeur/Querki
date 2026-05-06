package querki.display.rx

import rx._

import org.querki.gadgets.reactive.RxEmptyable

object QuerkiEmptyable {

  implicit def RxEmptyableText[T <: RxTextBase[_]]: RxEmptyable[T] = new RxEmptyable[T] {
    def rxEmpty(elem: T)(implicit ctx: Ctx.Owner) = Rx { elem.textOpt().isEmpty }
  }

  implicit val RxEmptyableRadio: RxEmptyable[RxRadio] = new RxEmptyable[RxRadio] {

    def rxEmpty(elem: RxRadio)(implicit ctx: Ctx.Owner) = Rx {
      val opt = elem.selectedOption()
      opt.isEmpty
    }
  }
}
