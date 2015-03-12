package org.querki.jquery

import scala.scalajs.js
import js.UndefOr
import org.scalajs.dom
import dom.{Element, Event}
import org.querki.jsext._

/**
 * The facade over an event from JQuery.
 */
trait JQueryEventObject extends Event {
  /**
   * An optional object of data passed to an event method when the current executing handler is bound.
   */
  def data: Any = js.native
  
  /**
   * The element where the currently-called jQuery event handler was attached.
   */
  def delegateTarget: Element = js.native
  
  /**
   * Returns whether event.preventDefault() was ever called on this event object.
   */
  def isDefaultPrevented(): Boolean = js.native
  
  /**
   * Returns whether event.stopImmediatePropagation() was ever called on this event object.
   */
  def isImmediatePropogationStopped(): Boolean = js.native
  
  /**
   * Returns whether event.stopPropagation() was ever called on this event object.
   */
  def isPropogationStopped(): Boolean = js.native
  
  /**
   * Indicates whether the META key was pressed when the event fired.
   */
  def metaKey: Boolean = js.native
  
  /**
   * The namespace specified when the event was triggered.
   */
  def namespace: String = js.native
  
  /**
   * The mouse position relative to the left edge of the document.
   */
  def pageX: Int = js.native
  
  /**
   * The mouse position relative to the top edge of the document.
   */
  def pageY: Int = js.native
  
  /**
   * If this method is called, the default action of the event will not be triggered.
   */
  override def preventDefault(): Unit = js.native
  
  /**
   * The other DOM element involved in the event, if any.
   */
  def relatedTarget: Element = js.native
  
  /**
   * The last value returned by an event handler that was triggered by this event, unless the value was undefined.
   */
  def result: UndefOr[Any] = js.native
  
  /**
   * Keeps the rest of the handlers from being executed and prevents the event from bubbling up the DOM tree.
   */
  override def stopImmediatePropagation(): Unit = js.native
  
  /**
   * Prevents the event from bubbling up the DOM tree, preventing any parent handlers from being notified of the event.
   */
  override def stopPropagation(): Unit = js.native
  
  /**
   * For key or mouse events, this property indicates the specific key or button that was pressed.
   */
  def which: Int = js.native
}

/**
 * Constructor facade for JQueryEventObject. See the documentation of the JQueryEventObject trait for more
 * information about the various constructor methods.
 */
object JQueryEventObject extends JQueryEventObjectBuilder(noOpts)
class JQueryEventObjectBuilder(val dict:OptMap) extends JSOptionBuilder[JQueryEventObject, JQueryEventObjectBuilder](new JQueryEventObjectBuilder(_)) {
  def data(v:Any) = jsOpt("data", v)
  def delegateTarget(v:Element) = jsOpt("delegateTarget", v)
  def namespace(v:String) = jsOpt("namespace", v)
  def relatedTarget(v:Element) = jsOpt("relatedTarget", v)
  def result(v:Any) = jsOpt("result", v)
  def pageX(v:Int) = jsOpt("pageX", v)
  def pageY(v:Int) = jsOpt("pageY", v)
  def which(v:Int) = jsOpt("which", v)
  def metaKey(v:Boolean) = jsOpt("metaKey", v)
}
