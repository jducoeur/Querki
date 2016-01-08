package querki.display

import scala.scalajs.js

import org.scalajs.dom.{raw => dom}
import org.querki.jquery._

import scalatags.JsDom.all._
import scalatags.JsDom.TypedTag

import models.Wikitext

import querki.globals._

import querki.comm._
import querki.data.{BasicThingInfo, ThingInfo}
import querki.util.ScalatagUtils

/**
 * Wrapper around Scalatags, to provide support for tracking and updating the rendered elements
 * as the underlying data changes.
 * 
 * If you need complex behaviour, subclass this and extend it. If you just need to be able to
 * access the DOM created by the rendered Scalatags, just use the Gadget(scalatags) entry point.
 */
trait Gadget[Output <: dom.Element] extends ManagedFrag[Output] with QuerkiUIUtils with EcologyMember {
  def ecology:Ecology
  
  /**
   * Concrete subclasses should fill this in with the actual guts of the Gadget.
   */
  def doRender():TypedTag[Output]
  
  def underlyingTag = doRender()
  
  def createFrag = underlyingTag.render
  
  /**
   * Focuses on the first useful thing in this Gadget.
   */
  def focus() = {
    elemOpt.foreach { e =>
      $(e).findFirst(canFocus(_)).map($(_).focus())
    }
  }
  
  /**
   * Indicates that something has changed significantly to alter the page layout, so the
   * Page should be recalculated as necessary.
   */
  def updatePage() = {
    val Pages = interface[querki.pages.Pages]
    Pages.findPageFor(this).map(_.reindex())
  }
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
class SimpleGadget(guts:scalatags.JsDom.TypedTag[dom.Element], hook: dom.Element => Unit)(implicit val ecology:Ecology) extends Gadget[dom.Element] {
  def doRender() = guts
  override def onCreate(e:dom.Element) = { hook(e) }
}

/**
 * Wrapper around a TypedTag. You don't need to specify this explicitly unless you need a hook -- there
 * is an implicit def in globals that transforms TypedTag into TypedGadget.
 */
class TypedGadget[Output <: dom.Element](guts:scalatags.JsDom.TypedTag[Output], hook: Output => Unit)(implicit val ecology:Ecology) extends Gadget[Output] {
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
  def apply(guts:scalatags.JsDom.TypedTag[dom.Element])(implicit ecology:Ecology) = new SimpleGadget(guts, { elem:dom.Element => })
  
  /**
   * Create a SimpleGadget from the given Scalatags. This is typically enough when all you need is
   * to get at the resulting DOM element.
   */
  def apply(guts:scalatags.JsDom.TypedTag[dom.Element], hook: dom.Element => Unit)(implicit ecology:Ecology) = new SimpleGadget(guts, hook)
}

/**
 * A trivial Gadget that you can register when you just want to hook some behavior into an Element
 * based on a Selector.
 */
class HookGadget(onHook:dom.Element => Unit)(implicit e:Ecology) extends HookedGadget[dom.Element](e) {
  def hook() = { onHook(elem) }
  
  // These should never be directly rendered:
  def doRender() = ???
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
    a(classes(Seq("btn", "btn-default", "btn-xs", "btn-primary", "_noPrint", "querki-icon-button") ++ addlCls),
      show)
  
  def icon(iconName:String) = i(classes(Seq("glyphicon", s"glyphicon-$iconName")))
  
  /**
   * Show a standard Querki icon button.
   */
  def iconButton(iconName:String, addlCls:Seq[String] = Seq.empty) = querkiButton(icon(iconName), addlCls)
  
  def thingUrl(thing:ThingInfo) = s"#!${thing.urlName.underlying}"
  
  def thingUrl(name:TID) = s"#!${name.underlying}"
  
  /**
   * A standard link to a Thing, if you're not trying to do anything odd with it.
   */
  def thingLink(thing:BasicThingInfo):TypedTag[dom.HTMLAnchorElement] =
    a(href:=thingUrl(thing), thing.displayName)
    
  implicit class jqGadgetExts(jq:JQuery) {
    def tidString(name:String) = TID(jq.data(name).asInstanceOf[String])
    
    /**
     * Goes through all of the passed-in elements and their descendants, recursively,
     * depth-first, looking for the first element that passes the given predicate.
     */
    def findFirst(pred:dom.Element => Boolean):Option[dom.Element] = {
      val thisLevel = jq.filter(pred)
      thisLevel.get(0).toOption.orElse {
        // Okay, nothing found here, so dive down a level, and try each one's children:
        val raws = jq.get()
        (Option.empty[dom.Element] /: raws) { (found, raw) =>
          found.orElse {
            $(raw.asInstanceOf[dom.Element]).children().findFirst(pred)
          }
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
