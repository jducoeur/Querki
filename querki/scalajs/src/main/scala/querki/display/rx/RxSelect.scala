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
 * 
 * Note that the options list is a reactive, so you can update the options easily. If you don't need
 * to do that, you can simply put everything in the mods list instead, and pass an empty Var for options.
 */
class RxSelect(options:Rx[Seq[Frag]], mods:Modifier*) extends Gadget[dom.HTMLSelectElement] {
  
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
  
  def doRender() = select(mods, options())
  
  def updateSelected() = { selectedOption() = curSelected }
  
  val obs = Obs(options, skipInitial=true) {
    $(elem).empty()
    options().map(_.render).map(opt => $(elem).append(opt))
    updateSelected()
  }
  
  override def onCreate(e:dom.HTMLSelectElement) = {
    $(e).change({ evt:JQueryEventObject => updateSelected() })
    updateSelected()
  }
}
object RxSelect {
  def apply(options:Rx[Seq[Frag]], mods:Modifier*) = new RxSelect(options, mods)
  def apply(mods:Modifier*) = new RxSelect(Var(Seq.empty), mods)
}
