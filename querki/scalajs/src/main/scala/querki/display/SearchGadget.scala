package querki.display

import org.scalajs.dom
import org.scalajs.jquery._
import scalatags.JsDom.all.{input => inp, _}

import querki.globals._

class SearchGadget extends Gadget[dom.HTMLInputElement] {
  def doRender() = inp(tpe:="text")
  
  override def onCreate(elem:dom.HTMLInputElement) = {
    // For now, we're just going to deal with it when the user hits return.
    // TODO: in the long run, can we do prompting, a la Google?
    
  }
}
