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
      println(s"""Text value is now "${element.value}"""")
    })
  }
  
  def doRender() =
    input(cls:="_textEdit", tpe:="text")
    
}
