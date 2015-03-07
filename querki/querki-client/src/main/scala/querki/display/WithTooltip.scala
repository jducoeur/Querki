package querki.display

import scala.scalajs.js
import org.scalajs.dom
import scalatags.JsDom.all._

import org.querki.facades.bootstrap._

import querki.globals._

/**
 * "Tooltip-izes" the given tag, which is typically a label.
 * 
 * TODO: at some point, we should probably broaden this to be hookable with the _withTooltip class. But
 * that really should not require a full InputGadget; I think the InputGadgets registry needs to be
 * refactored first.
 */
class WithTooltip[Output <: dom.Element](tag:scalatags.JsDom.TypedTag[Output], tooltip:String = "") extends Gadget[Output] {
  override def onCreate(elem:Output) = $(elem).tooltip(TooltipOptions.delay(250).title(tooltip))
  def doRender() = tag
}
