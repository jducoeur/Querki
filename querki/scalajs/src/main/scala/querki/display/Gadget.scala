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
  def elem = _elem.get
  
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
