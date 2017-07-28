package querki.display.rx

import org.scalajs.dom
import org.querki.jquery._
import rx._
import querki.globals._
import org.querki.gadgets.core.ManagedFrag

/**
 * A reactive text Modifier, which lets you place some Text into Scalatags, and have it change
 * when the underlying Rx changes. It does this by setting the .text() jQuery property of the
 * parent node.
 * 
 * Note that this requires an Rx[String] specifically, to keep things simple. Use rx.ops.map to
 * turn other types into Strings.
 */
class RxTextFrag(rx:Rx[String])(implicit ctx:Ctx.Owner) extends ManagedFrag[dom.Text] {
  def createFrag = dom.document.createTextNode("")
  
  lazy val obs = rx.trigger {
    parentOpt.foreach { parent =>
      $(parent).text(rx.now)
    }
  }
  
  def onRendered(node:dom.Text):Unit = {
    obs
  }
}
