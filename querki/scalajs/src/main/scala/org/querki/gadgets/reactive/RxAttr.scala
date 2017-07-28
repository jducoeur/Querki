package org.querki.gadgets.reactive

import scala.scalajs.js
import org.scalajs.dom
import org.querki.jquery._
import scalatags.JsDom.all._
import rx._
  
// TODO: the view bounds used below are all a bit smelly -- view bounds aren't exactly
// a favorite mechanism in idiomatic Scala. Can we redo this as a typeclass?
private [reactive] class RxAttrBase[T <% AttrVal, R <: Rx[T]](implicit ctx:Ctx.Owner) extends AttrValue[R] {
  def apply(t:dom.Element, a:Attr, v:R):Unit = {
    v.trigger {
      $(t).attr(a.name, v.now)
    }
  }
}

/**
 * Mechanism for using an Rx as a Scalatags AttrValue.
 * 
 * Note that this is used implicitly -- just import querki.display.rx._, and it
 * will add the ability to use Rx-defined attribute values.
 */
class RxAttr[T <% AttrVal](implicit ctx:Ctx.Owner) extends RxAttrBase[T, Rx[T]]
class VarAttr[T <% AttrVal](implicit ctx:Ctx.Owner) extends RxAttrBase[T, Var[T]]
class RxDynAttr[T <% AttrVal](implicit ctx:Ctx.Owner) extends RxAttrBase[T, Rx.Dynamic[T]]

private [reactive] class RxStyleBase[T <% StyleVal, R <: Rx[T]](implicit ctx:Ctx.Owner) extends StyleValue[R] {
  def apply(t:dom.Element, s:Style, v:R):Unit = {
    v.trigger {
      $(t).css(s.jsName, v.now)
    }
  }
}

class RxStyle[T <% StyleVal](implicit ctx:Ctx.Owner) extends RxStyleBase[T, Rx[T]]
class VarStyle[T <% StyleVal](implicit ctx:Ctx.Owner) extends RxStyleBase[T, Var[T]]
class RxDynStyle[T <% StyleVal](implicit ctx:Ctx.Owner) extends RxStyleBase[T, Rx.Dynamic[T]]
