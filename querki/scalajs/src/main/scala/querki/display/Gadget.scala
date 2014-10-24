package querki.display

import scala.scalajs.js

import org.scalajs.dom

import scalatags.JsDom.all._

import models.Wikitext

import querki.globals._

import querki.comm._
import querki.data.ThingInfo
import querki.util.ScalatagUtils

/**
 * Wrapper around Scalatags, to provide support for tracking and updating the rendered elements
 * as the underlying data changes.
 */
trait Gadget[Output <: dom.Element] extends scalatags.jsdom.Frag with ScalatagUtils {
  /**
   * Concrete subclasses should fill this in with the actual guts of the Gadget.
   */
  def doRender():scalatags.JsDom.TypedTag[Output]
  
  lazy val underlyingTag = doRender()
  
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
  
  var _elem:Option[Output] = None
  def elemOpt = _elem
  def elem = _elem.get
  /**
   * Slam the element for this Gadget. You should only call this iff the element was actually called from
   * an external mechanism (eg, via QText), and you're building this Gadget around that element.
   * 
   * This is intentionally designed for chaining, for ease of use -- it returns this Gadget.
   */
  def setElem(e:dom.Element):this.type = {
    _elem = Some(e.asInstanceOf[Output])
    this
  }
  
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
