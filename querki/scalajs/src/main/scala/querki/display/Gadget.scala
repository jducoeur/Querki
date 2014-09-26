package querki.display

import org.scalajs.dom

import scalatags.JsDom.all._

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
  
  var _elem:Option[Output] = None
  
  def render:Output = {
    val result = underlyingTag.render
    _elem = Some(result)
    result
  }
}
