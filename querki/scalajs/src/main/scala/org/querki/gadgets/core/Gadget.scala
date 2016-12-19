package org.querki.gadgets.core

import org.scalajs.dom

import org.querki.jquery._

/**
 * Wrapper around Scalatags, to provide support for tracking and updating the rendered elements
 * as the underlying data changes.
 * 
 * If you need complex behaviour, subclass this and extend it. If you just need to be able to
 * access the DOM created by the rendered Scalatags, just use the Gadget(scalatags) entry point.
 */
trait Gadget[Output <: dom.Element] extends ManagedFrag[Output] {
  /**
   * Concrete subclasses should fill this in with the actual guts of the Gadget.
   * 
   * Note that this can be filled with a conventional Scalatags TypedTag thanks to an
   * implicit conversion in globals.
   */
  def doRender():ManagedFrag[Output]
  
  def underlyingTag = doRender()
  
  def createFrag = underlyingTag.render
  
  /**
   * Focuses on the first useful thing in this Gadget.
   * 
   * TODO: this should be in a typeclass.
   */
  def focus() = {
    elemOpt.foreach { e =>
      findFirst($(e), canFocus(_)).map($(_).focus())
    }
  }
  
  /**
   * Concrete Gadgets can override this to perform actions after we've created the actual Element.
   */
  def onCreate(elem:Output) = {}
  
  def onRendered(e:Output):Unit = {
    GadgetLookup.annotateGadget(this)
    onCreate(e)
  }
  
  /**
   * Goes through all of the passed-in elements and their descendants, recursively,
   * depth-first, looking for the first element that passes the given predicate.
   */
  private def findFirst(jq:JQuery, pred:dom.Element => Boolean):Option[dom.Element] = {
    val thisLevel = jq.filter(pred)
    thisLevel.get(0).toOption.orElse {
      // Okay, nothing found here, so dive down a level, and try each one's children:
      val raws = jq.get()
      (Option.empty[dom.Element] /: raws) { (found, raw) =>
        found.orElse {
          findFirst($(raw.asInstanceOf[dom.Element]).children(), pred)
        }
      }
    }
  }
  
  /**
   * This is our rough heuristic for whether something is "focusable". We use this for stuff
   * like computing tab order, actually choosing what to focus on, and so on.
   * 
   * Our definition of "focusable" is a bit rough and ready: it is stuff the user *might* want
   * to tab to. We expect this to evolve a bit.
   * 
   * TBD: is there really no standard way to do this? I haven't found one yet.
   * 
   * TODO: lift this into a typeclass.
   */
  def canFocus(e:dom.Element):Boolean = {
    e.tagName match {
      case "A" | "BUTTON" | "INPUT" | "SELECT" | "TEXTAREA" => {
        // True unless it is disabled:
        // TODO: is this working? I don't think it is doing so, at least not consistently:
        $(e).prop("disabled").map(_ != true ).getOrElse(true)
      }
      case _ => false
    }    
  }
}

/**
 * This variant of Gadget is particularly useful when you're not trying to do anything complex, just
 * have a handle to the resulting elem. Usually accessed as Gadget(...).
 */
class SimpleGadget(guts:scalatags.JsDom.TypedTag[dom.Element], hook: dom.Element => Unit) extends Gadget[dom.Element] {
  def doRender() = guts
  override def onCreate(e:dom.Element) = { hook(e) }
}

object Gadget {
  /**
   * Create a SimpleGadget from the given Scalatags. This is typically enough when all you need is
   * to get at the resulting DOM element.
   * 
   * You shouldn't often need to call this explicitly; there is an implicit def in globals that will
   * do it for you.
   */
  def apply(guts:scalatags.JsDom.TypedTag[dom.Element]) = new SimpleGadget(guts, { elem:dom.Element => })
  
  /**
   * Create a SimpleGadget from the given Scalatags. This is typically enough when all you need is
   * to get at the resulting DOM element.
   */
  def apply(guts:scalatags.JsDom.TypedTag[dom.Element], hook: dom.Element => Unit) = new SimpleGadget(guts, hook)
}

/**
 * Wrapper around a TypedTag. You don't need to specify this explicitly -- there
 * is an implicit def in globals that transforms TypedTag into TypedGadget.
 */
class TypedGadget[Output <: dom.Element](guts:scalatags.JsDom.TypedTag[Output]) extends Gadget[Output] {
  def doRender() = guts
  // We need to override this in order to break what would otherwise be an infinite loop:
  override def createFrag = guts.render
}
