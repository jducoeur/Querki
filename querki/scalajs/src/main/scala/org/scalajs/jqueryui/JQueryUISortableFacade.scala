package org.scalajs.jqueryui

import scala.scalajs.js
import js.{Dynamic, UndefOr, undefined => undef}
import js.JSConverters._
import org.scalajs.jquery._

trait JQueryUISortableFacade extends js.Object {
  def sortable(options:SortableOptions):JQuery = ???
  def sortable(cmd:String):Any = ???
}

object SortableOptions {
  def apply(
    stop:UndefOr[js.Function2[JQueryEventObject, SortChangeUI, Any]] = undef
  ) = {
    Dynamic.literal(
      stop = stop
    ).asInstanceOf[SortableOptions]
  }
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

/**
 * Options that can be passed into a Sortable.
 * 
 * TODO: this is currently a partial API, missing most of the Events.
 */
trait SortableOptions extends js.Object {
  // TODO: this is a union type. How should we deal with it?
//  var appendTo:UndefOr[???]
  
  /**
   * If defined, the items can be dragged only horizontally or vertically. Possible values: "x", "y".
   */
  var axis:UndefOr[String] = _
  
  /**
   * Prevents sorting if you start on elements matching the selector.
   * 
   * Default: "input,textarea,button,select,option"
   */
  var cancel:UndefOr[Selector] = _
  
  /**
   * A selector of other sortable elements that the items from this list should be connected to.
   * This is a one-way relationship, if you want the items to be connected in both directions, 
   * the connectWith option must be set on both sortable elements.
   */
  var connectWith:UndefOr[Selector] = _
  
  /**
   * Defines a bounding box that the sortable items are constrained to while dragging.
   * 
   * Note: The element specified for containment must have a calculated width and height (though 
   * it need not be explicit). For example, if you have float: left sortable children and specify 
   * containment: "parent" be sure to have float: left on the sortable/parent container as well 
   * or it will have height: 0, causing undefined behavior.
   * 
   * TODO: this is actually a union, and can take Element instead.
   */
  var containment:UndefOr[Selector] = _
  
  /**
   * Defines the cursor that is being shown while sorting.
   * 
   * Default: "auto"
   */
  var cursor:UndefOr[String] = _
  
  /**
   * Moves the sorting element or helper so the cursor always appears to drag from the same position. 
   * Coordinates can be given as a hash using a combination of one or two keys: { top, left, right, bottom }.
   */
  var cursorAt:UndefOr[js.Object] = _

  /**
   * Time in milliseconds to define when the sorting should start. Adding a delay helps preventing 
   * unwanted drags when clicking on an element.
   */
  var delay:UndefOr[Int] = _
  
  /**
   * Disables the sortable if set to true.
   */
  var disabled:UndefOr[Boolean] = _
  
  /**
   * Tolerance, in pixels, for when sorting should start. If specified, sorting will not start until 
   * after mouse is dragged beyond distance. Can be used to allow for clicks on elements within a handle.
   */
  var distance:UndefOr[Int] = _
  
  /**
   * If false, items from this sortable can't be dropped on an empty connect sortable (see the connectWith option).
   * 
   * Default: true
   */
  var dropOnEmpty:UndefOr[Boolean] = _
  
  /**
   * If true, forces the helper to have a size.
   * 
   * Default: false
   */
  var forceHelperSize:UndefOr[Boolean] = _
  
  /**
   * If true, forces the placeholder to have a size.
   */
  var forcePlaceholderSize:UndefOr[Boolean] = _
  
  /**
   * Snaps the sorting element or helper to a grid, every x and y pixels. Array values: [ x, y ].
   */
  var grid:UndefOr[js.Array[Int]] = _
  
  /**
   * Restricts sort start click to the specified element.
   * 
   * TODO: should be union with Element.
   */
  var handle:UndefOr[Selector] = _
  
  // TODO: this one is messy and weird. Think about how to expose it.
//  var helper:UndefOr[String] = _
  
  /**
   * Specifies which items inside the element should be sortable.
   * 
   * Default: "> *"
   */
  var items:UndefOr[Selector] = _
  
  /**
   * Defines the opacity of the helper while sorting. From 0.01 to 1.
   */
  var opacity:UndefOr[Float] = _
  
  /**
   * A class name that gets applied to the otherwise white space.
   */
  var placeholder:UndefOr[String] = _
  
  /**
   * Whether the sortable items should revert to their new positions using a smooth animation.
   * 
   * Multiple types supported:
   *   Boolean: When set to true, the items will animate with the default duration.
   *   Number: The duration for the animation, in milliseconds.
   *   
   * TODO: we don't yet support setting this to true.
   */
  var revert:UndefOr[Int] = _
  
  /**
   * If set to true, the page scrolls when coming to an edge.
   * 
   * Default: true
   */
  var scroll:UndefOr[Boolean] = _
  
  /**
   * Defines how near the mouse must be to an edge to start scrolling.
   * 
   * Default: 20
   */
  var scrollSensitivity:UndefOr[Int] = _
  
  /**
   * The speed at which the window should scroll once the mouse pointer gets within the scrollSensitivity distance.
   * 
   * Default: 20
   */
  var scrollSpeed:UndefOr[Int] = _
  
  /**
   * This event is triggered during sorting.
   */
  var stop:UndefOr[js.Function2[JQueryEventObject, SortChangeUI, Any]] = _
  
  /**
   * Specifies which mode to use for testing whether the item being moved is hovering over another item. Possible values:
   *   "intersect": The item overlaps the other item by at least 50%.
   *   "pointer": The mouse pointer overlaps the other item.
   *   
   * Default: "intersect"
   */
  var tolerance:UndefOr[String] = _
  
  /**
   * Z-index for element/helper while being sorted.
   * 
   * Default: 1000
   */
  var zIndex:UndefOr[Int] = _
}
