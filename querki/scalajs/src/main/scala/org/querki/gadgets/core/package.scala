package org.querki.gadgets

import org.scalajs.dom.html.Element

import scalatags.JsDom.TypedTag

package object core {  
  implicit def tag2Gadget[Output <: Element](
    guts:TypedTag[Output]
  ):Gadget[Output]
    = new TypedGadget[Output](guts)
}
