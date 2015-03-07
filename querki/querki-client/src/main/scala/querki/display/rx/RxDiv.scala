package querki.display.rx

import scala.scalajs.js
import js.JSConverters._
import org.scalajs.dom
import org.scalajs.jquery._
import scalatags.JsDom.all._
import rx._
import rx.ops._

import querki.globals._

import querki.display.Gadget

/**
 * A div whose contents can be updated reactively.
 * 
 * TODO: the way that renderedDiv works here really ought to replace the rendered method in
 * ManagedFrag. Indeed, I really should re-examine the way things work in this light, noting
 * the relationship between doRender() and obs below.
 */
case class RxDiv(rxGuts:Rx[Seq[Gadget[_]]], base:Modifier*) extends Gadget[dom.HTMLDivElement] {
  
  def doRender() = divTag()
  
  lazy val divTag = Rx(name="divTag") { div(base, rxGuts()) }
  lazy val divRx = divTag.map(_.render)
  
  lazy val obs = Obs(divRx) {
    val newContent = divRx()
    $(elem).replaceWith(newContent)
    setElem(newContent)
    elemRx() = Some(newContent)
  }
  
  /**
   * Listeners can pay attention to changes to this, if they want to fire after updates:
   */
  lazy val elemRx = Var[Option[dom.Element]](None)
  
  override def onCreate(e:dom.HTMLDivElement) = obs
}
