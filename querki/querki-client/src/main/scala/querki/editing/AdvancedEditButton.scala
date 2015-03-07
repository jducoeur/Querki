package querki.editing

import scala.scalajs.js
import org.scalajs.dom
import org.scalajs.jquery._
import scalatags.JsDom.all._

import querki.globals._

import querki.display.input.InputGadget

class AdvancedEditButton(implicit e:Ecology) extends InputGadget[dom.HTMLSpanElement](e) {
  
  lazy val Editing = interface[Editing]
  
  lazy val editor = $(elem).parents("._instanceEditor")
  lazy val tid = editor.tidString("thingid")
  
  def values = ???
  
  def doRender() = span(cls:="_advancedCommentButton", "x")
  
  def hook() = {
    $(elem).on("click", null, null, { evt:JQueryEventObject => Editing.advancedEditorFactory.showPage(tid); 1:js.Any })
  }

}
