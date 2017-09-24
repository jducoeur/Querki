package querki.display.rx

import org.scalajs.dom.{raw => dom}
import org.querki.gadgets._
import org.querki.jquery._
import scalatags.JsDom.all._
import rx._

import querki.globals._

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
class RxSelect(options:Rx[Seq[Frag]], emptyText:Option[String], mods:Modifier*)(implicit val ecology:Ecology, ctx:Ctx.Owner) extends Gadget[dom.HTMLSelectElement] with RxThingSelector {
  
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
  lazy val selectedValOpt:Rx[Option[String]] = selectedOption.map(_.map(_.valueString).filter(_.length > 0))
  lazy val selectedVal:Rx[String] = selectedValOpt.map(_.getOrElse(""))
  lazy val selectedTIDOpt:Rx[Option[TID]] = selectedValOpt.map(_.map(TID(_)))
  lazy val selectedTID:Rx[TID] = selectedVal.map(TID(_))
  
  /**
   * This is all of the options, including the "empty" option at the top iff one was specified.
   */
  lazy val allOptions = Rx { 
    emptyText match {
      case Some(text) => option(value:="", text) +: options()
      case None => options()
    } 
  }
  
  /**
   * Non-empty iff this RxSelect has a non-empty value. That way, you can build an Rx based on whether
   * this is set or not.
   */
  lazy val selectedWithTID:Rx[Option[(RxSelect, TID)]] = selectedTIDOpt.map(_.map(v => (this, v)))
  
  def doRender() = select(mods, cls:="form-control", allOptions.now)
  
  def setValue(v:String) = {
    $(elem).value(v)
    updateSelected()
  }
  
  private def updateSelected() = { selectedOption() = curSelected }
  
  private val obs = allOptions.triggerLater {
    $(elem).empty()
    // TBD: this cast is ugly, and results from the fact that options is Seq[Frag], which
    // is awfully loose. Can/should we tighten up that signature?
    allOptions.now.map(_.render).map(opt => $(elem).append(opt.asInstanceOf[dom.Element]))
    updateSelected()
  }
  
  override def onCreate(e:dom.HTMLSelectElement) = {
    $(e).change({ e:dom.Element => updateSelected() })
    updateSelected()
  }
}
object RxSelect {
  def apply(options:Rx[Seq[Frag]], emptyText:String, mods:Modifier*)(implicit ecology:Ecology, ctx:Ctx.Owner) = new RxSelect(options, Some(emptyText), mods)
  def apply(options:Rx[Seq[Frag]], mods:Modifier*)(implicit ecology:Ecology, ctx:Ctx.Owner) = new RxSelect(options, None, mods)
  def apply(mods:Modifier*)(implicit ecology:Ecology, ctx:Ctx.Owner) = new RxSelect(Var(Seq.empty), None, mods)
}
