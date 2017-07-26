package org.querki.gadgets.reactive

import scala.scalajs.js.|
import org.scalajs.dom

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
  implicit def rxAttr[T <% AttrVal] = new RxAttr[T]
  implicit def varAttr[T <% AttrVal] = new VarAttr[T]
  
  implicit def rxStyle[T <% StyleVal] = new RxStyle[T]
  implicit def varStyle[T <% StyleVal] = new VarStyle[T]
}
