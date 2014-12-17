package querki.display

import scala.scalajs.js

import org.scalajs.dom
import org.scalajs.jquery._

import scalatags.JsDom.all._
import scalatags.JsDom.TypedTag

import models.Wikitext

import querki.globals._

import querki.comm._
import querki.data.BasicThingInfo
import querki.display.input.InputGadget
import querki.util.ScalatagUtils

/**
 * Wrapper around Scalatags, to provide support for tracking and updating the rendered elements
 * as the underlying data changes.
 * 
 * If you need complex behaviour, subclass this and extend it. If you just need to be able to
 * access the DOM created by the rendered Scalatags, just use the Gadget(scalatags) entry point.
 */
trait Gadget[Output <: dom.Element] extends ManagedFrag[Output] with QuerkiUIUtils {
  /**
   * Concrete subclasses should fill this in with the actual guts of the Gadget.
   */
  def doRender():TypedTag[Output]
  
  def underlyingTag = doRender()
  
  def createFrag = underlyingTag.render
}

/**
 * Similar to Gadget, but this is specifically for Gadgets that wrap around other Gadgets. Only use
 * this if you need the alternate doRender() signature -- that is, when doRender's top level is itself
 * another Gadget.
 */
trait MetaGadget[Output <: dom.Element] extends ManagedFrag[Output] with ScalatagUtils with QuerkiUIUtils {
  def doRender():Gadget[Output]
  
  def underlyingTag = doRender()
  
  def createFrag = underlyingTag.render
}

/**
 * This variant of Gadget is particularly useful when you're not trying to do anything complex, just
 * have a handle to the resulting elem. Usually accessed as Gadget(...).
 */
class SimpleGadget(guts:scalatags.JsDom.TypedTag[dom.Element], hook: dom.Element => Unit) extends Gadget[dom.Element] {
  def doRender() = guts
  override def onCreate(e:dom.Element) = { hook(e) }
}

/**
 * Wrapper around a TypedTag. You don't need to specify this explicitly unless you need a hook -- there
 * is an implicit def in globals that transforms TypedTag into TypedGadget.
 */
class TypedGadget[Output <: dom.Element](guts:scalatags.JsDom.TypedTag[Output], hook: Output => Unit) extends Gadget[Output] {
  def doRender() = guts
  override def onCreate(e:Output) = { hook(e) }
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
 * A trivial Gadget that you can register when you just want to hook some behavior into an Element
 * based on a Selector.
 * 
 * TODO: this really shouldn't be an InputGadget conceptually, but we need that for the hooking:
 */
class HookGadget(onHook:dom.Element => Unit)(implicit e:Ecology) extends InputGadget[dom.Element](e) {
  def hook() = { onHook(elem) }
  
  // These should never be directly rendered:
  def doRender() = ???
  def values = ???
}

trait QuerkiUIUtils extends ScalatagUtils {
  /**
   * Render some wikitext from the server.
   * 
   * This should be changing rapidly and dramatically, becoming a much more complex Gadget unto itself.
   */
  def wikitext(w:Wikitext) = raw(w.display.html.toString)
  
  /**
   * Show a standard Querki button, displaying whatever contents are given.
   */
  def querkiButton(show:Modifier, addlCls:Seq[String] = Seq.empty) =
    a(classes(Seq("btn", "btn-mini", "btn-primary", "_noPrint", "querki-icon-button") ++ addlCls),
      show)
  
  def icon(iconName:String) = i(classes(Seq("icon-white", s"icon-$iconName")))
  
  /**
   * Show a standard Querki icon button.
   */
  def iconButton(iconName:String, addlCls:Seq[String] = Seq.empty) = querkiButton(icon(iconName), addlCls)
  
  def thingUrl(name:TID) = s"#${name.underlying}"
  
  /**
   * A standard link to a Thing, if you're not trying to do anything odd with it.
   */
  def thingLink(thing:BasicThingInfo):TypedTag[dom.HTMLAnchorElement] =
    a(href:=thingUrl(thing), thing.displayName)
    
  implicit class jqGadgetExts(jq:JQuery) {
    def tidString(name:String) = TID(jq.data(name).asInstanceOf[String])
  }
}
