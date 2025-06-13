package org.querki.gadgets.reactive

import scala.scalajs.js.|
import org.scalajs.dom
import rx._

import org.querki.jquery._

/**
 * These are implicit constructors for RxAttr and RxStyle.
 * 
 * TODO: can we rewrite these to use context bounds instead of view bounds? I think it
 * should be possible...
 */
trait Implicits {
  /**
   * This is the evidence required to use an Rx as an AttrValue in Scalatags.
   */
  implicit def rxAttr[T <% AttrVal](implicit ctx:Ctx.Owner) = new RxAttr[T]
  implicit def varAttr[T <% AttrVal](implicit ctx:Ctx.Owner) = new VarAttr[T]
  implicit def rxDynAttr[T <% AttrVal](implicit ctx:Ctx.Owner) = new RxDynAttr[T]
  
  implicit def rxStyle[T <% StyleVal](implicit ctx:Ctx.Owner) = new RxStyle[T]
  implicit def varStyle[T <% StyleVal](implicit ctx:Ctx.Owner) = new VarStyle[T]
  implicit def rxDynStyle[T <% StyleVal](implicit ctx:Ctx.Owner) = new RxDynStyle[T]
}
