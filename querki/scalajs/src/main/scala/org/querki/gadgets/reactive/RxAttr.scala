package org.querki.gadgets.reactive

import org.scalajs.dom
import org.querki.jquery._
import scalatags.JsDom.all._
import rx._

private[reactive] class RxAttrBase[T, R <: Rx[T]](implicit ev: T => AttrVal, ctx: Ctx.Owner) extends AttrValue[R] {

  def apply(
    t: dom.Element,
    a: Attr,
    v: R
  ): Unit = {
    v.trigger {
      $(t).attr(a.name, v.now)
      ()
    }
  }
}

/**
 * Mechanism for using an Rx as a Scalatags AttrValue.
 *
 * Note that this is used implicitly -- just import querki.display.rx._, and it
 * will add the ability to use Rx-defined attribute values.
 */
class RxAttr[T](implicit ev: T => AttrVal, ctx: Ctx.Owner) extends RxAttrBase[T, Rx[T]]
class VarAttr[T](implicit ev: T => AttrVal, ctx: Ctx.Owner) extends RxAttrBase[T, Var[T]]
class RxDynAttr[T](implicit ev: T => AttrVal, ctx: Ctx.Owner) extends RxAttrBase[T, Rx.Dynamic[T]]

private[reactive] class RxStyleBase[T, R <: Rx[T]](implicit ev: T => StyleVal, ctx: Ctx.Owner) extends StyleValue[R] {

  def apply(
    t: dom.Element,
    s: Style,
    v: R
  ): Unit = {
    v.trigger {
      $(t).css(s.jsName, v.now)
      ()
    }
  }
}

class RxStyle[T](implicit ev: T => StyleVal, ctx: Ctx.Owner) extends RxStyleBase[T, Rx[T]]
class VarStyle[T](implicit ev: T => StyleVal, ctx: Ctx.Owner) extends RxStyleBase[T, Var[T]]
class RxDynStyle[T](implicit ev: T => StyleVal, ctx: Ctx.Owner) extends RxStyleBase[T, Rx.Dynamic[T]]
