package querki.display

import scala.scalajs.js

import org.scalajs.dom

import scalatags.JsDom.all._

import models.Wikitext

import querki.globals._

import querki.comm._
import querki.data.ThingInfo

/**
 * Wrapper around Scalatags, to provide support for tracking and updating the rendered elements
 * as the underlying data changes.
 */
trait Gadget[Output <: dom.Element] extends scalatags.jsdom.Frag {
  /**
   * Concrete subclasses should fill this in with the actual guts of the Gadget.
   */
  def doRender():scalatags.JsDom.TypedTag[Output]
  
  lazy val underlyingTag = doRender()
  
  /**
   * Utility, to make it easier to define data attributes.
   */
  def data(name:String):Attr = scalatags.generic.Attr(s"data-$name")

  /**
   * Utility function; this is often useful for wrapping complex expressions that produce Modifiers, which
   * often otherwise don't trigger the implicits properly. Often needed around if statements, in particular.
   */
  def MSeq(xs:Modifier*) = Vector[Modifier](xs)
  
  /**
   * Render some wikitext from the server.
   * 
   * This should be changing rapidly and dramatically, becoming a much more complex Gadget unto itself.
   */
  def wikitext(w:Wikitext) = raw(w.display.html.toString)
  
  /**
   * Convenience function for composing classes in Gadgets and functions.
   */
  def classes(cs:Seq[String]) = cls:=cs.mkString(" ")
  
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
  
  var _elem:Option[Output] = None
  
  /**
   * Concrete Gadgets can override this to perform actions after we've created the actual Element.
   */
  def onCreate(elem:Output) = {}
  
  def render:Output = {
    val result = underlyingTag.render
    _elem = Some(result)
    onCreate(result)
    result
  }
}
