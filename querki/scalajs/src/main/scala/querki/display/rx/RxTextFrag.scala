package querki.display.rx

import org.scalajs.dom
import rx._
import querki.globals._
import querki.display.ManagedFrag

/**
 * A reactive text Modifier, which lets you place some Text into Scalatags, and have it change
 * when the underlying Rx changes. It does this by setting the .text() jQuery property of the
 * parent node.
 * 
 * Note that this requires an Rx[String] specifically, to keep things simple. Use rx.ops.map to
 * turn other types into Strings.
 */
class RxTextFrag(rx:Rx[String]) extends ManagedFrag[dom.Text] {
  def createFrag = dom.document.createTextNode("")
  
  lazy val obs = Obs(rx) {
    parentOpt.foreach { parent =>
      $(parent).text(rx())
    }
  }
  
  override def onCreate(txt:dom.Text) = obs
}
