package querki.display

import org.scalajs.dom.{raw => dom}

import querki.globals._

/**
 * A trivial Gadget that you can register when you just want to hook some behavior into an Element
 * based on a Selector.
 */
class HookGadget(onHook:dom.Element => Unit)(implicit e:Ecology) extends HookedGadget[dom.Element](e) {
  def hook() = { onHook(elem) }
  
  // These should never be directly rendered:
  def doRender() = ???
}
