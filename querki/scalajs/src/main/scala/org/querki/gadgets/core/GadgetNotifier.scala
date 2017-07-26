package org.querki.gadgets.core

import org.scalajs.dom.html.Element

/**
 * This typeclass is used to express the notion of something that can receive notifications
 * from Gadgets.
 */
trait GadgetNotifier[T] {
  /**
   * Called when a Gadget has altered in a way that may require recomputing layout,
   * tab order, and that sort of thing. Generally implies that nodes have been added
   * or removed from the displayed page.
   */
  def layoutChanged[Output <: Element](g:Gadget[Output]):Unit
}
