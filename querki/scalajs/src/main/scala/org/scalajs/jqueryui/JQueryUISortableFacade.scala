package org.scalajs.jqueryui

import scala.scalajs.js
import js.{Dynamic, UndefOr, undefined => undef}
import js.JSConverters._
import org.scalajs.dom
import org.scalajs.jquery._
import org.scalajs.ext._

trait JQueryUISortableFacade extends js.Object {
  def sortable(options:SortableOptions):JQuery = ???
  def sortable(cmd:String):Any = ???
}

/**
 * Options that can be passed into a Sortable.
 * 
 * You create the options using the methods on the companion object.
 * 
 * TODO: this is currently a partial API, missing most of the Events.
 */
trait SortableOptions extends js.Object
object SortableOptions extends SortableOptionBuilder(noOpts)
class SortableOptionBuilder(val dict:OptMap) extends JSOptionBuilder[SortableOptions, SortableOptionBuilder](new SortableOptionBuilder(_)) {
  
  /**
   * Defines where the helper that moves with the mouse is being appended to during the drag (for example, to resolve overlap/zIndex issues).
   * 
   * @param v A jQuery object containing the element to append the helper to.
   */
  def appendTo(v:JQuery) = jsOpt("appendTo", v)
  /**
   * Defines where the helper that moves with the mouse is being appended to during the drag (for example, to resolve overlap/zIndex issues).
   * 
   * @param v The element to append the helper to.
   */
  def appendTo(v:dom.Element) = jsOpt("appendTo", v)
  /**
   * Defines where the helper that moves with the mouse is being appended to during the drag (for example, to resolve overlap/zIndex issues).
   * 
   * @param v A selector specifying which element to append the helper to. The string "parent" will cause the helper to be a sibling of the sortable item.
   */
  def appendTo(v:String) = jsOpt("appendTo", v)
  
  /**
   * If defined, the items can be dragged only horizontally or vertically. Possible values: "x", "y".
   */
  def axis(v:String) = jsOpt("axis", v)
  
  /**
   * Prevents sorting if you start on elements matching the selector.
   * 
   * Default: "input,textarea,button,select,option"
   */
  def cancel(v:Selector) = jsOpt("cancel", v)
  
  /**
   * A selector of other sortable elements that the items from this list should be connected to.
   * This is a one-way relationship, if you want the items to be connected in both directions, 
   * the connectWith option must be set on both sortable elements.
   */
  def connectWith(v:Selector) = jsOpt("connectWith", v)
  
  /**
   * Defines a bounding box that the sortable items are constrained to while dragging.
   * 
   * Note: The element specified for containment must have a calculated width and height (though 
   * it need not be explicit). For example, if you have float: left sortable children and specify 
   * containment: "parent" be sure to have float: left on the sortable/parent container as well 
   * or it will have height: 0, causing undefined behavior.
   * 
   * @param v An element to use as the container.
   */
  def containment(v:dom.Element) = jsOpt("containment", v)
  /**
   * Defines a bounding box that the sortable items are constrained to while dragging.
   * 
   * Note: The element specified for containment must have a calculated width and height (though 
   * it need not be explicit). For example, if you have float: left sortable children and specify 
   * containment: "parent" be sure to have float: left on the sortable/parent container as well 
   * or it will have height: 0, causing undefined behavior.
   * 
   * @param v A selector specifying an element to use as the container.
   *    *Or*, A string identifying an element to use as the container. Possible values: "parent", "document", "window".
   */
  def containment(v:Selector) = jsOpt("containment", v)
  
  /**
   * Defines the cursor that is being shown while sorting.
   * 
   * Default: "auto"
   */
  def cursor(v:String) = jsOpt("cursor", v)
  
  /**
   * Moves the sorting element or helper so the cursor always appears to drag from the same position. 
   * Coordinates can be given as a hash using a combination of one or two keys: { top, left, right, bottom }.
   */
  def cursorAt(v:js.Object) = jsOpt("cursorAt", v)

  /**
   * Time in milliseconds to define when the sorting should start. Adding a delay helps preventing 
   * unwanted drags when clicking on an element.
   */
  def delay(v:Int) = jsOpt("delay", v)
  
  /**
   * Disables the sortable if set to true.
   */
  def disabled(v:Boolean) = jsOpt("disabled", v)
  
  /**
   * Tolerance, in pixels, for when sorting should start. If specified, sorting will not start until 
   * after mouse is dragged beyond distance. Can be used to allow for clicks on elements within a handle.
   */
  def distance(v:Int) = jsOpt("distance", v)
  
  /**
   * If false, items from this sortable can't be dropped on an empty connect sortable (see the connectWith option).
   * 
   * Default: true
   */
  def dropOnEmpty(v:Boolean) = jsOpt("dropOnEmpty", v)
  
  /**
   * If true, forces the helper to have a size.
   * 
   * Default: false
   */
  def forceHelperSize(v:Boolean) = jsOpt("forceHelperSize", v)
  
  /**
   * If true, forces the placeholder to have a size.
   */
  def forcePlaceholderSize(v:Boolean) = jsOpt("forcePlaceholderSize", v)
  
  /**
   * Snaps the sorting element or helper to a grid, every x and y pixels. Array values: [ x, y ].
   */
  def grid(v:js.Array[Int]) = jsOpt("grid", v)
  
  /**
   * Restricts sort start click to the specified element.
   * 
   * @param v Selector of the element to use.
   */
  def handle(v:Selector) = jsOpt("handle", v)
  /**
   * Restricts sort start click to the specified element.
   * 
   * @param v The element to use.
   */
  def handle(v:dom.Element) = jsOpt("handle", v)

  /**
   * Allows for a helper element to be used for dragging display.
   * 
   * @param v If set to "clone", then the element will be cloned and the clone will be dragged.
   *   
   */
  def helper(v:String) = jsOpt("helper", v)
  /**
   * Allows for a helper element to be used for dragging display.
   * 
   * @param v A function that will return a DOMElement to use while dragging. The function receives the event and the element being sorted.
   */
  def helper(v:js.Function0[dom.Element]) = jsOpt("helper", v)
  
  /**
   * Specifies which items inside the element should be sortable.
   * 
   * Default: "> *"
   */
  def items(v:Selector) = jsOpt("items", v)
  
  /**
   * Defines the opacity of the helper while sorting. From 0.01 to 1.
   */
  def opacity(v:Float) = jsOpt("opacity", v)
  
  /**
   * A class name that gets applied to the otherwise white space.
   */
  def placeholder(v:String) = jsOpt("placeholder", v)
  
  /**
   * Whether the sortable items should revert to their new positions using a smooth animation.
   * 
   * Default: false
   * 
   * @param v When set to true, the items will animate with the default duration.
   */
  def revert(v:Boolean) = jsOpt("revert", v)
  /**
   * Whether the sortable items should revert to their new positions using a smooth animation.
   * 
   * @param v The duration for the animation, in milliseconds.
   */
  def revert(v:Int) = jsOpt("revert", v)
  
  /**
   * If set to true, the page scrolls when coming to an edge.
   * 
   * Default: true
   */
  def scroll(v:Boolean) = jsOpt("scroll", v)
  
  /**
   * Defines how near the mouse must be to an edge to start scrolling.
   * 
   * Default: 20
   */
  def scrollSensitivity(v:Int) = jsOpt("scrollSensitivity", v)
  
  /**
   * The speed at which the window should scroll once the mouse pointer gets within the scrollSensitivity distance.
   * 
   * Default: 20
   */
  def scrollSpeed(v:Int) = jsOpt("scrollSpeed", v)
  
  /**
   * This event is triggered during sorting.
   */
  def stop(v:js.Function2[JQueryEventObject, SortChangeUI, Any]) = jsOpt("stop", v)
  
  /**
   * Specifies which mode to use for testing whether the item being moved is hovering over another item. Possible values:
   *   "intersect": The item overlaps the other item by at least 50%.
   *   "pointer": The mouse pointer overlaps the other item.
   *   
   * Default: "intersect"
   */
  def tolerance(v:String) = jsOpt("tolerance", v)
  
  /**
   * Z-index for element/helper while being sorted.
   * 
   * Default: 1000
   */
  def zIndex(v:Int) = jsOpt("zIndex", v)
}

trait SortPosition extends js.Object {
  var top:Int = _
  var left:Int = _
}

/**
 * This object is returned by a number of events.
 * 
 * NOTE: the fields are all currently listed as UndefOr. The JQUI documentation implies that
 * they are all always set, but I am suspicious of whether that is actually true, so I'm playing
 * it safe. This needs further examination.
 */
trait SortChangeUI extends js.Object {
  /**
   * The jQuery object representing the helper being sorted.
   */
  var helper:UndefOr[JQuery] = _
  
  /**
   * The jQuery object representing the current dragged element.
   */
  var item:UndefOr[JQuery] = _
  
  /**
   * The current absolute position of the helper represented as { top, left }.
   */
  var offset:UndefOr[SortPosition] = _
  
  /**
   * The current position of the helper represented as { top, left }.
   */
  var position:UndefOr[SortPosition] = _
  
  /**
   * The original position of the element represented as { top, left }.
   */
  var originalPosition:UndefOr[SortPosition] = _
  
  /**
   * The sortable that the item comes from if moving from one sortable to another.
   */
  var sender:UndefOr[JQuery] = _
  
  /**
   * The jQuery object representing the element being used as a placeholder.
   */
  var placeholder:UndefOr[JQuery] = _
}
