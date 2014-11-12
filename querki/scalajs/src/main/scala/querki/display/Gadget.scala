package querki.display

import scala.scalajs.js

import org.scalajs.dom

import scalatags.JsDom.all._
import scalatags.JsDom.TypedTag

import models.Wikitext

import querki.globals._

import querki.comm._
import querki.data.ThingInfo
import querki.util.ScalatagUtils

/**
 * Wrapper around Scalatags, to provide support for tracking and updating the rendered elements
 * as the underlying data changes.
 * 
 * If you need complex behaviour, subclass this and extend it. If you just need to be able to
 * access the DOM created by the rendered Scalatags, just use the Gadget(scalatags) entry point.
 */
trait Gadget[Output <: dom.Element] extends ManagedFrag[Output] with ScalatagUtils {
  /**
   * Concrete subclasses should fill this in with the actual guts of the Gadget.
   */
  def doRender():TypedTag[Output]
  
  lazy val underlyingTag = doRender()
  
  def createFrag = underlyingTag.render
  
  // TODO: split all of these out into some sort of QuerkiUIUtils:
  
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
  
  /**
   * Shortcut for fetching the URL of a Thing.
   */
  def thingUrl(thing:ThingInfo):String = {
    thingUrl(thing.urlName)
  }
  
  def thingUrl(name:String) = s"#$name"
  
  /**
   * A standard link to a Thing, if you're not trying to do anything odd with it.
   */
  def thingLink(thing:ThingInfo):TypedTag[dom.HTMLAnchorElement] =
    a(href:=thingUrl(thing), thing.displayName)
}

/**
 * This variant of Gadget is particularly useful when you're not trying to do anything complex, just
 * have a handle to the resulting elem. Usually accessed as Gadget(...).
 */
class SimpleGadget(guts:scalatags.JsDom.TypedTag[dom.Element]) extends Gadget[dom.Element] {
  def doRender() = guts
}

object Gadget {
  /**
   * Create a SimpleGadget from the given Scalatags. This is typically enough when all you need is
   * to get at the resulting DOM element.
   */
  def apply(guts:scalatags.JsDom.TypedTag[dom.Element]) = new SimpleGadget(guts)
}
