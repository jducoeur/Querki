package org.querki.gadgets

import org.scalajs.dom

import scalatags.JsDom.TypedTag

package object core {
  
  type AnyNode <: dom.Node
  type AnyFrag = ManagedFrag[AnyNode]
  
  implicit def tag2Gadget[Output <: dom.Element](
    guts:TypedTag[Output]
  ):Gadget[Output]
    = new TypedGadget[Output](guts)

}