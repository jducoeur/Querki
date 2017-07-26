package org.querki.gadgets.reactive

import scala.scalajs.js
import org.scalajs.dom
import org.querki.jquery._
import scalatags.JsDom.all._
import rx._
  
// TODO: the view bounds used below are all a bit smelly -- view bounds aren't exactly
// a favorite mechanism in idiomatic Scala. Can we redo this as a typeclass?
private [reactive] class RxAttrBase[T <% AttrVal, R <: Rx[T]] extends AttrValue[R] {
  def apply(t:dom.Element, a:Attr, v:R):Unit = {
    Obs(v) {
      $(t).attr(a.name, v())
    }
  }
}

/**
 * Mechanism for using an Rx as a Scalatags AttrValue.
 * 
 * Note that this is used implicitly -- just import querki.display.rx._, and it
 * will add the ability to use Rx-defined attribute values.
 */
class RxAttr[T <% AttrVal] extends RxAttrBase[T, Rx[T]]
class VarAttr[T <% AttrVal] extends RxAttrBase[T, Var[T]]

private [reactive] class RxStyleBase[T <% StyleVal, R <: Rx[T]] extends StyleValue[R] {
  def apply(t:dom.Element, s:Style, v:R):Unit = {
    Obs(v) {
      $(t).css(s.jsName, v())
    }
  }
}

class RxStyle[T <% StyleVal] extends RxStyleBase[T, Rx[T]]
class VarStyle[T <% StyleVal] extends RxStyleBase[T, Var[T]]
