package org.querki.gadgets.reactive

import rx._

import org.querki.jquery._

trait Implicits {
  implicit def rxAttr[T](implicit ev: T => AttrVal, ctx: Ctx.Owner) = new RxAttr[T]
  implicit def varAttr[T](implicit ev: T => AttrVal, ctx: Ctx.Owner) = new VarAttr[T]
  implicit def rxDynAttr[T](implicit ev: T => AttrVal, ctx: Ctx.Owner) = new RxDynAttr[T]

  implicit def rxStyle[T](implicit ev: T => StyleVal, ctx: Ctx.Owner) = new RxStyle[T]
  implicit def varStyle[T](implicit ev: T => StyleVal, ctx: Ctx.Owner) = new VarStyle[T]
  implicit def rxDynStyle[T](implicit ev: T => StyleVal, ctx: Ctx.Owner) = new RxDynStyle[T]
}
