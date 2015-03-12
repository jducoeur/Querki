package querki.display.rx

import scala.scalajs.js
import org.scalajs.dom
import org.querki.jquery._
import rx._
import querki.globals._
import querki.display.ManagedFrag

/**
 * TODO: this is a horrible hacked workaround for the lack of true union types.
 * We really want AttrVal to be a proper union in the signature of attr(); lacking
 * that, we're shielding the js.Any of this version behind the firewall of AttrVal.
 */
trait AttrExt extends js.Object {
  def attr(attributeName:String, v:js.Any):JQuery = js.native
}
  
/**
 * Defines an attribute, suitable for embedding in Scalatags, whose value is based on a
 * reactive.
 */
class RxAttr[T <% AttrVal](name:String, rx:Rx[T]) extends ManagedFrag[dom.Attr] {
  def createFrag = dom.document.createAttribute(name)
  
  lazy val obs = Obs(rx) {
    parentOpt.foreach { parent =>
      $(parent).asInstanceOf[AttrExt].attr(name, rx().asInstanceOf[js.Any])
    }
  }
  
  override def onCreate(attr:dom.Attr) = obs

  // Note that, unlike an ordinary ManagedFrag, this does *not* delegate to super.applyTo().
  // That's because the default Frag in the Scalatag's JS world does an appendTo(), which
  // makes no sense for an attribute. So just as Scalatag's JS Attrs have their own
  // overrides, so must we. In our case, instead of actually applying anything to the
  // parent, we kick off rendering, which causes the obs to come into existence:
  override def applyTo(parent:dom.Element) = {
    parentOpt = Some(parent)
    render
  }
}
object RxAttr {
  def apply[T <% AttrVal](name:String, rx:Rx[T]) = new RxAttr(name, rx)
}
