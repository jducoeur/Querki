package querki.display.input

import org.scalajs.dom
import scalatags.JsDom.all._

import querki.globals._

import querki.display._

abstract class InputGadget(val ecology:Ecology) extends Gadget[dom.Element] with EcologyMember {
  
  type elemType <: dom.Element
  
  def rawElement:dom.Element
  
  lazy val element = rawElement.asInstanceOf[elemType]
  
  /**
   * Hook whatever events are appropriate for this Gadget.
   */
  def hook():Unit
}
