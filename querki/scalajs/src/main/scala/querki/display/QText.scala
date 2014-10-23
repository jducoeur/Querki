package querki.display

import org.scalajs.dom
import scalatags.JsDom.all._

import models.Wikitext

import querki.globals._

class QText(text:Wikitext)(implicit val ecology:Ecology) extends Gadget[dom.Element] with EcologyMember {
  
  lazy val InputGadgets = interface[input.InputGadgets]

  override def onCreate(root:dom.Element) = {
    InputGadgets.hookRawGadgets(root)
  }
  
  def doRender() = {
    // TODO: putting this in a div() is WrongityWrongWrong, since it sometimes might be span-ish.
    // How do we make this appropriately general? Conceptually, a Large Text is a div, and a Text is
    // a span; do we need to distinguish that way somehow?
    div(wikitext(text))
  }
  
}
