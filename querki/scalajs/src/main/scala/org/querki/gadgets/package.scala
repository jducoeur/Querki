package org.querki

import org.scalajs.dom
import org.scalajs.dom.html.Element
import org.querki.jquery._

/**
 * This is the root of the Gadgets library. This level exists solely to give you the
 * convenience import org.querki.gadgets._, which pulls in all the commonly-useful
 * types. Look in subpackages for the details and power features.
 */
package object gadgets extends reactive.Implicits {
  type ManagedFrag[Output <: dom.Node] = core.ManagedFrag[Output]
  type Gadget[Output <: Element] = core.Gadget[Output]
  val Gadget = core.Gadget
  type GadgetRef[G <: Gadget[_]] = core.GadgetRef[G]
  val GadgetRef = core.GadgetRef
  type TypedGadget[Output <: Element] = core.TypedGadget[Output]
  type SimpleGadget = core.SimpleGadget
  
  type AnyNode <: dom.Node
  type AnyFrag = ManagedFrag[AnyNode]
}
