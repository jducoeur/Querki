package querki.editing

import scala.scalajs.js
import org.scalajs.dom
import org.querki.jquery._
import scalatags.JsDom.all._

import querki.globals._

import querki.display.HookedGadget

class AdvancedEditButton(implicit e:Ecology) extends HookedGadget[dom.html.Span](e) {
  
  lazy val Editing = interface[Editing]
  
  lazy val editor = $(elem).parents("._instanceEditor")
  lazy val tid = editor.tidString("thingid")
  
  def doRender() = span(cls:="_advancedCommentButton", "x")
  
  def hook() = {
    $(elem).click { () => Editing.advancedEditorFactory.showPage(tid) }
  }

}
