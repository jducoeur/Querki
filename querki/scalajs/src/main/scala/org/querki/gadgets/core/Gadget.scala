package org.querki.gadgets.core

import org.scalajs.dom.html.Element

import org.querki.squery.Focusable
import Focusable._
import org.querki.squery.Findable._

/**
 * Wrapper around Scalatags, to provide support for tracking and updating the rendered elements
 * as the underlying data changes.
 * 
 * If you need complex behaviour, subclass this and extend it. If you just need to be able to
 * access the DOM created by the rendered Scalatags, just use the Gadget(scalatags) entry point.
 */
trait Gadget[Output <: Element] extends ManagedFrag[Output] {
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
   * Convenience function for operating on the Element, if this Gadget has been rendered.
   * 
   * Yes, this deliberately squashes Output to Element for purposes of the function call.
   * That is typically good enough, and is kinder to the sQuery type inference.
   * 
   * TBD: this suggests that sQuery should be smarter about Element subclasses.
   */
  def mapElementOrElse[R](default:R, f:Element => R):R = {
    elemOpt.map(f).getOrElse(default)
  }
  
  /**
   * Concrete Gadgets can override this to perform actions after we've created the actual Element.
   */
  def onCreate(elem:Output) = {}
  
  def onRendered(e:Output):Unit = {
    GadgetLookup.annotateGadget(this)
    onCreate(e)
  }
}

/**
 * This variant of Gadget is particularly useful when you're not trying to do anything complex, just
 * have a handle to the resulting elem. Usually accessed as Gadget(...).
 */
class SimpleGadget(guts:scalatags.JsDom.TypedTag[Element], hook: Element => Unit) extends Gadget[Element] {
  def doRender() = guts
  override def onCreate(e:Element) = { hook(e) }
}

object Gadget {
  /**
   * Create a SimpleGadget from the given Scalatags. This is typically enough when all you need is
   * to get at the resulting DOM element.
   * 
   * You shouldn't often need to call this explicitly; there is an implicit def in globals that will
   * do it for you.
   */
  def apply(guts:scalatags.JsDom.TypedTag[Element]) = new SimpleGadget(guts, { elem:Element => })
  
  /**
   * Create a SimpleGadget from the given Scalatags. This is typically enough when all you need is
   * to get at the resulting DOM element.
   */
  def apply(guts:scalatags.JsDom.TypedTag[Element], hook: Element => Unit) = new SimpleGadget(guts, hook)
  
  implicit def GadgetFocusable[G <: Gadget[_]] = new Focusable[G] {
    /**
     * The Gadget is considered focusable iff it contains something focusable.
     */
    def canFocus(g:G):Boolean = {
      g.mapElementOrElse(false, _.findFirst(_.canFocus).isDefined)
    }
    
    /**
     * Focus on the first sub-element that *can* be focused.
     */
    def focus(g:G):Unit = {
      g.mapElementOrElse((), _.findFirst(_.canFocus).map(_.focus()))
    }
  }
}

/**
 * Wrapper around a TypedTag. You don't need to specify this explicitly -- there
 * is an implicit def in globals that transforms TypedTag into TypedGadget.
 */
class TypedGadget[Output <: Element](guts:scalatags.JsDom.TypedTag[Output]) extends Gadget[Output] {
  def doRender() = guts
  // We need to override this in order to break what would otherwise be an infinite loop:
  override def createFrag = guts.render
}
