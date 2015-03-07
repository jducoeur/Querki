package querki.display.rx

import org.scalajs.dom
import org.scalajs.jquery._
import scalatags.JsDom.all._
import rx._
import rx.ops._

import querki.globals._

import querki.display.Gadget

trait RxThingSelector {
  def selectedText:Rx[String]
  def selectedTID:Rx[TID]
}

/**
 * A wrapper around the select element, which provides reactive that let you keep an eye on the current
 * selection.
 * 
 * Note that the options list is a reactive, so you can update the options easily. If you don't need
 * to do that, you can simply put everything in the mods list instead, and pass an empty Var for options.
 * 
 * It is legal for the options to include one (usually at the top) with "" as its value. That is considered
 * to be the "not set" state.
 */
class RxSelect(options:Rx[Seq[Frag]], mods:Modifier*) extends Gadget[dom.HTMLSelectElement] with RxThingSelector {
  
  private def curSelected = {
    elemOpt.map(e => $(e).find("option:selected"))
  }
  /**
   * Note that this says which option is selected, even if it is the "not set" one.
   */
  lazy val selectedOption = Var[Option[JQuery]](None)
  lazy val selectedTextOpt = Rx { selectedOption().map(_.text()) }
  lazy val selectedText = selectedTextOpt.map(_.getOrElse(""))
  /**
   * This variant of selectedVal filters out the empty string.
   */
  lazy val selectedValOpt = Rx { selectedOption().map(_.valueString).filter(_.length > 0) }
  lazy val selectedVal = selectedValOpt.map(_.getOrElse(""))
  lazy val selectedTIDOpt = selectedValOpt.map(_.map(TID(_)))
  lazy val selectedTID = selectedVal.map(TID(_))
  
  /**
   * Non-empty iff this RxSelect has a non-empty value. That way, you can build an Rx based on whether
   * this is set or not.
   */
  lazy val selectedWithTID = Rx { selectedTIDOpt().map(v => (this, v)) }
  
  def doRender() = select(mods, options())
  
  def setValue(v:String) = {
    $(elem).value(v)
    updateSelected()
  }
  
  private def updateSelected() = { selectedOption() = curSelected }
  
  private val obs = Obs(options, skipInitial=true) {
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
