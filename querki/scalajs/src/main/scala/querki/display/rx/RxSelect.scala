package querki.display.rx

import org.scalajs.dom
import org.scalajs.jquery._
import scalatags.JsDom.all._
import rx._
import rx.ops._

import querki.globals._

import querki.display.Gadget

/**
 * A wrapper around the select element, which provides reactive that let you keep an eye on the current
 * selection.
 */
class RxSelect(mods:Modifier*) extends Gadget[dom.HTMLSelectElement] {
  
  private def curSelected = {
    elemOpt.map(e => $(e).find("option:selected"))
  }
  lazy val selectedOption = Var[Option[JQuery]](None)
  lazy val selectedText = Rx { selectedOption().map(_.text()).getOrElse("") }
  /**
   * This variant of selectedVal filters out the empty string.
   */
  lazy val selectedValOpt = Rx { selectedOption().map(_.valueString).filter(_.length > 0) }
  lazy val selectedVal = selectedValOpt.map(_.getOrElse(""))
  
  def doRender() = select(mods)
  
  def updateSelected() = { selectedOption() = curSelected }
  
  override def onCreate(e:dom.HTMLSelectElement) = {
    $(e).change({ evt:JQueryEventObject => updateSelected() })
    updateSelected()
  }
}
