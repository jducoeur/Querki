package querki.display.rx

import scala.scalajs.js
import org.scalajs.dom
import org.querki.jquery._
import scalatags.JsDom.all._
import rx._
import querki.globals._
import querki.display.ManagedFrag
  
private [rx] class RxAttrBase[T <% AttrVal, R <: Rx[T]] extends AttrValue[R] {
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
