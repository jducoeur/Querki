package org.scalajs.jqueryui

import scala.scalajs.js
import js.{Dynamic, UndefOr, undefined => undef}
import js.JSConverters._
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

object SortableOptions extends JSOptionBuilder[SortableOptions] {
  // TODO: this is a union type. How should we deal with it?
//  val appendTo:???
  
  /**
   * If defined, the items can be dragged only horizontally or vertically. Possible values: "x", "y".
   */
  val axis = jsOpt[String]("axis")
  
  /**
   * Prevents sorting if you start on elements matching the selector.
   * 
   * Default: "input,textarea,button,select,option"
   */
  val cancel = jsOpt[Selector]("cancel")
  
  /**
   * A selector of other sortable elements that the items from this list should be connected to.
   * This is a one-way relationship, if you want the items to be connected in both directions, 
   * the connectWith option must be set on both sortable elements.
   */
  val connectWith = jsOpt[Selector]("connectWith")
  
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
  val containment = jsOpt[Selector]("containment")
  
  /**
   * Defines the cursor that is being shown while sorting.
   * 
   * Default: "auto"
   */
  val cursor = jsOpt[String]("cursor")
  
  /**
   * Moves the sorting element or helper so the cursor always appears to drag from the same position. 
   * Coordinates can be given as a hash using a combination of one or two keys: { top, left, right, bottom }.
   */
  val cursorAt = jsOpt[js.Object]("cursorAt")

  /**
   * Time in milliseconds to define when the sorting should start. Adding a delay helps preventing 
   * unwanted drags when clicking on an element.
   */
  val delay = jsOpt[Int]("delay")
  
  /**
   * Disables the sortable if set to true.
   */
  val disabled = jsOpt[Boolean]("disabled")
  
  /**
   * Tolerance, in pixels, for when sorting should start. If specified, sorting will not start until 
   * after mouse is dragged beyond distance. Can be used to allow for clicks on elements within a handle.
   */
  val distance = jsOpt[Int]("distance")
  
  /**
   * If false, items from this sortable can't be dropped on an empty connect sortable (see the connectWith option).
   * 
   * Default: true
   */
  val dropOnEmpty = jsOpt[Boolean]("dropOnEmpty")
  
  /**
   * If true, forces the helper to have a size.
   * 
   * Default: false
   */
  val forceHelperSize = jsOpt[Boolean]("forceHelperSize")
  
  /**
   * If true, forces the placeholder to have a size.
   */
  val forcePlaceholderSize = jsOpt[Boolean]("forcePlaceholderSize")
  
  /**
   * Snaps the sorting element or helper to a grid, every x and y pixels. Array values: [ x, y ].
   */
  val grid = jsOpt[js.Array[Int]]("grid")
  
  /**
   * Restricts sort start click to the specified element.
   * 
   * TODO: should be union with Element.
   */
  val handle = jsOpt[Selector]("handle")
  
  // TODO: this one is messy and weird. Think about how to expose it.
//  var helper:UndefOr[String] = _
  
  /**
   * Specifies which items inside the element should be sortable.
   * 
   * Default: "> *"
   */
  val items = jsOpt[Selector]("items")
  
  /**
   * Defines the opacity of the helper while sorting. From 0.01 to 1.
   */
  val opacity = jsOpt[Float]("opacity")
  
  /**
   * A class name that gets applied to the otherwise white space.
   */
  val placeholder = jsOpt[String]("placeholder")
  
  /**
   * Whether the sortable items should revert to their new positions using a smooth animation.
   * 
   * Multiple types supported:
   *   Boolean: When set to true, the items will animate with the default duration.
   *   Number: The duration for the animation, in milliseconds.
   *   
   * TODO: we don't yet support setting this to true.
   */
  val revert = jsOpt[Int]("revert")
  
  /**
   * If set to true, the page scrolls when coming to an edge.
   * 
   * Default: true
   */
  val scroll = jsOpt[Boolean]("scroll")
  
  /**
   * Defines how near the mouse must be to an edge to start scrolling.
   * 
   * Default: 20
   */
  val scrollSensitivity = jsOpt[Int]("scrollSensitivity")
  
  /**
   * The speed at which the window should scroll once the mouse pointer gets within the scrollSensitivity distance.
   * 
   * Default: 20
   */
  val scrollSpeed = jsOpt[Int]("scrollSpeed")
  
  /**
   * This event is triggered during sorting.
   */
  val stop = jsOpt[js.Function2[JQueryEventObject, SortChangeUI, Any]]("stop")
  
  /**
   * Specifies which mode to use for testing whether the item being moved is hovering over another item. Possible values:
   *   "intersect": The item overlaps the other item by at least 50%.
   *   "pointer": The mouse pointer overlaps the other item.
   *   
   * Default: "intersect"
   */
  val tolerance = jsOpt[String]("tolerance")
  
  /**
   * Z-index for element/helper while being sorted.
   * 
   * Default: 1000
   */
  val zIndex = jsOpt[Int]("zIndex")  
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
