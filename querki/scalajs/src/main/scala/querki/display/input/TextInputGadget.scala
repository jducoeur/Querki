package querki.display.input

import org.scalajs.dom
import org.scalajs.jquery._
import scalatags.JsDom.all._

import querki.globals._

class TextInputGadget(implicit e:Ecology) extends InputGadget[dom.HTMLInputElement](e) {
  
  override def save() = {
    saveChange(List(elem.value))
  }  
  
  // TBD: do we need an unhook, to avoid leaks?
  def hook() = {
    $(elem).change({ event:JQueryEventObject => save() })
    
    $(elem).keypress({ (evt:JQueryEventObject) => beginChanges() })
  }
  
  def doRender() =
    input(cls:="_textEdit", tpe:="text")
    
}
object TextInputGadget {
  def apply(rawElement:dom.Element)(implicit e:Ecology) = {
    (new TextInputGadget).setElem(rawElement)
  }
}

trait AutosizeFacade extends JQuery {
  def autosize():JQuery = ???
}
object AutosizeFacade {
  implicit def jq2Autosize(jq:JQuery):AutosizeFacade = jq.asInstanceOf[AutosizeFacade]
}
import AutosizeFacade._

class LargeTextInputGadget(implicit e:Ecology) extends InputGadget[dom.HTMLTextAreaElement](e) {
  
  override def save() = {
    saveChange(List(elem.value))
  }
  
  // TBD: do we need an unhook, to avoid leaks?
  def hook() = {
    // Mark LargeTextInputs as autosized.
    // We specifically need to *not* apply autosize to the template elements, or else it won't
    // successfully apply to them when we actually instantiate them.
    // Note that we define the :notUnder selector in PageManager:
    $(elem).filter(":notUnder(.inputTemplate)").autosize()
    
    $(elem).change({ (evt:JQueryEventObject) => save() })
    
    $(elem).keypress({ (evt:JQueryEventObject) => beginChanges() })
  }
  
  def doRender() =
    textarea(cls:="_largeTextEdit")
    
}
object LargeTextInputGadget {
  def apply(rawElement:dom.Element)(implicit e:Ecology) = {
    (new LargeTextInputGadget).setElem(rawElement)
  }
}
