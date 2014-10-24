package querki.display.input

import scala.scalajs.js
import js._
import org.scalajs.dom
import org.scalajs.jquery._
import scalatags.JsDom.all._

import querki.globals._

class TextInputGadget(implicit e:Ecology) extends InputGadget[dom.HTMLInputElement](e) {
  
  def values = List(elem.value)
  
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

trait JQueryEventEnhanced extends js.Object {
  // This should be a standard part of JQueryEventObject, IMO:
  def ctrlKey:UndefOr[Int] = ???
}
object JQueryEventEnhanced {
  implicit def jqe2Enhanced(evt:JQueryEventObject):JQueryEventEnhanced = evt.asInstanceOf[JQueryEventEnhanced]
}
import JQueryEventEnhanced._

class LargeTextInputGadget(implicit e:Ecology) extends InputGadget[dom.HTMLTextAreaElement](e) {
  
  def values = List(elem.value)
  
  // TBD: do we need an unhook, to avoid leaks?
  def hook() = {
    // Mark LargeTextInputs as autosized.
    // We specifically need to *not* apply autosize to the template elements, or else it won't
    // successfully apply to them when we actually instantiate them.
    // Note that we define the :notUnder selector in PageManager:
    $(elem).filter(":notUnder(.inputTemplate)").autosize()
    
    $(elem).change({ (evt:JQueryEventObject) => save() })
    
    // Intercept ctrl-s, and save the value of this text. This is a bit horrible, but
    // necessary in order to work cross-browser.
    // TBD: should we do this at the Page level, so that it does the right thing anywhere
    // on the page? If nothing else, it would block the annoying popup.
    $(elem).keydown({ (evt:JQueryEventObject) =>
      val metaKey = evt.metaKey.asInstanceOf[UndefOr[Int]]
      if ((metaKey.isDefined || evt.ctrlKey.isDefined)
          && (evt.which.toChar.toString.toLowerCase == "s")) 
      {
        evt.preventDefault()
        save()
      }
    })
    
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
