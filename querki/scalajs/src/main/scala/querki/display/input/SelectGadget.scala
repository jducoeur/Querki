package querki.display.input

import org.scalajs.dom
import org.querki.jquery._

import querki.globals._

class SelectGadget(implicit e:Ecology) extends InputGadget[dom.HTMLSelectElement](e)  {

  // IMPORTANT: this currently does not allow for multi-select! Value() returns an array if
  // multi-select is turned on!
  def values = List($(elem).find("option:selected").value().asInstanceOf[String])
  
  def hook() = {
    $(elem).change({ e:dom.Element => save() })    
  }
  
  def doRender() = ???
}
