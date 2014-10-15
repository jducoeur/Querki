package querki.display.input

import org.scalajs.dom
import org.scalajs.jquery._
import scalatags.JsDom.all._

import querki.globals._

class TextInputGadget(val rawElement:dom.Element)(implicit e:Ecology) extends InputGadget(e) {
  
  type elemType = dom.HTMLInputElement
  
  // TBD: do we need an unhook, to avoid leaks?
  def hook() = {
    $(element).change({ event:JQueryEventObject =>
      saveChange(List(element.value))
    })
  }
  
  def doRender() =
    input(cls:="_textEdit", tpe:="text")
    
}

trait AutosizeFacade extends JQuery {
  def autosize():JQuery = ???
}
object AutosizeFacade {
  implicit def jq2Autosize(jq:JQuery):AutosizeFacade = jq.asInstanceOf[AutosizeFacade]
}
import AutosizeFacade._

class LargeTextInputGadget(val rawElement:dom.Element)(implicit e:Ecology) extends InputGadget(e) {
  
  type elemType = dom.HTMLTextAreaElement
  
  // TBD: do we need an unhook, to avoid leaks?
  def hook() = {
    // Mark LargeTextInputs as autosized.
    // We specifically need to *not* apply autosize to the template elements, or else it won't
    // successfully apply to them when we actually instantiate them.
    // Note that we define the :notUnder selector in PageManager:
    $(element).filter(":notUnder(.inputTemplate)").autosize()
    
    $(element).change({ event:JQueryEventObject =>
      saveChange(List(element.value))
    })
  }
  
  def doRender() =
    textarea(cls:="_largeTextEdit")
    
}
