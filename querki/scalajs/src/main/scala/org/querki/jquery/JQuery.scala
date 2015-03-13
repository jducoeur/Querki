package org.querki.jquery

import scala.scalajs.js
import js.UndefOr
import js.annotation.JSName

import org.scalajs.dom
import dom.Element

/**
 * This is a particularly important pseudo-union type. Selector is a common parameter type
 * in jQuery, meaning essentially a filter for choosing some elements. It can be a string
 * describing a kind of node (using a CSS-ish syntax), an Element, or an Array of Elements.
 * 
 * The actual types you can pass in to a Selector are defined by the implicit defs in
 * jquery.Defs. Note that the jQuery API documentation is *extremely* inconsistent about
 * how it treats the term "Selector" -- sometimes it just uses the term to mean Strings,
 * sometimes it means all of the possible types. We use this type for signatures that appear
 * to accept Strings, Elements and Arrays of Elements.
 * 
 * TODO: many of the signatures below should be tweaked to use Selector, once we've proved
 * that works!
 */
sealed trait Selector extends js.Any

/**
 * A facade for the main jQuery object.
 * 
 * This is a reimplementation, very loosely based on the existing scala-js-jquery. It aims to be much
 * more strongly and precisely typed, while being as literal a translation of the functionality of jQuery
 * as possible.
 * 
 * TODO: as of this writing, this is *quite* incomplete; I am only adding functions as I use them. Pull
 * requests are greatly welcomed. In particular, we are lacking many overloads -- I've added some of them,
 * but many jQuery functions have a considerable number of potential overloads.
 * 
 * Many parameters are polymorphic. In the case of Selector, which is used a *lot*, we've pulled that out
 * into a pseudo-union type, using implicit def trickery to make it work. That functions pretty well, but
 * unfortunately can't be generalized: if we do that too often, we wind up with implicit conflicts because
 * of multiple implicit paths from, eg, String to js.Any. So I'm still looking for a better approach to
 * defining these union types. We might yet resort to Scalaz for this, but I'm reluctant to introduce such
 * a bit dependency.
 * 
 * NOTE: discussion on scalajs Gitter, 1/28/15, says that facades should *return* Any, but
 * *take* js.Any *if* the Javascript is going to process the value in any way. This is the guiding principle here.
 * 
 * Also: when a facade function takes a property bag, if it is understood to be name/value pairs
 * in JS, declare it as js.Dictionary[T]. Often, we can constrain T; if not, just put js.Any, and it
 * is at least explicit that it is name/value pairs.
 */
trait JQuery extends js.Object {
  /**
   * Adds the specified class(es) to each of the set of matched elements.
   */
  def addClass(classNames:String):JQuery = js.native
  def addClass(func:js.ThisFunction2[Element, Int, String, String]):JQuery = js.native
  
  /**
   * Insert content, specified by the parameter, after each element in the set of matched elements.
   */
  def after(content:String):JQuery = js.native
  def after(content:Element):JQuery = js.native
  def after(content:JQuery):JQuery = js.native
  def after(content:Array[Element]):JQuery = js.native
  
  /**
   * Insert content, specified by the parameter, to the end of each element in the set of matched elements.
   * 
   * The content can be any number of elements, each one an HTML String *or* an Element *or* an Array
   * *or* a jQuery. There is no clean way to build a strongly-typed Scala signature for this.
   */
  def append(content:js.Any*):JQuery = js.native
  def append(func:js.ThisFunction2[Element, Int, String, js.Any]):JQuery = js.native
  
  /**
   * Insert every element in the set of matched elements to the end of the target.
   */
  def appendTo(target:String):JQuery = js.native
  def appendTo(target:Element):JQuery = js.native
  def appendTo(target:JQuery):JQuery = js.native
  
  /**
   * Get the value of an attribute for the first element in the set of matched elements.
   * 
   * Note that this returns UndefOr -- it is entirely legal for this to return undefined if
   * the attribute is not present, and that causes things to crash if it is not UndefOr.
   */
  def attr(attributeName:String):UndefOr[String] = js.native
  def attr(attributeName:String, v:String):JQuery = js.native
  def attr(attributeName:String, v:Int):JQuery = js.native
  def attr(attributeName:String, v:Boolean):JQuery = js.native
  def attr(attributes:js.Dictionary[String]):JQuery = js.native
  /**
   * TODO: this overload really shouldn't allow you to set Any, but we run into erasure problems
   * if we have separate overloads for String, Int and Boolean. Is there a good way to tighten this?
   */
  def attr(attributeName:String, func:js.ThisFunction2[Element, Int, String, Any]):JQuery = js.native
  
  /**
   * Bind an event handler to the "change" JavaScript event, or trigger that event on an element.
   * 
   * NOTE: the jQuery documentation is very fuzzy on this point, but implies in an example that "this" gets
   * set appropriately.
   */
  def change(handler:js.ThisFunction0[Element, Any]):JQuery = js.native
  def change(handler:js.ThisFunction1[Element, JQueryEventObject, Any]):JQuery = js.native
  def change(handler:js.Function1[JQueryEventObject, Any]):JQuery = js.native
  def change(eventData:Any, handler:js.ThisFunction1[Element, JQueryEventObject, Any]):JQuery = js.native
  def change():JQuery = js.native
  
  /**
   * Get the children of each element in the set of matched elements, optionally filtered by a selector.
   */
  def children(selector:String):JQuery = js.native
  def children():JQuery = js.native
    
  /**
   * Bind an event handler to the "click" JavaScript event.
   * 
   * This is the simpler version, and usually what you want.
   */
  def click(func:js.ThisFunction0[Element, Any]):JQuery = js.native
  def click(func:js.ThisFunction1[Element, JQueryEventObject, Any]):JQuery = js.native
  def click(func:js.Function1[JQueryEventObject, Any]):JQuery = js.native
  
  /**
   * Create a deep copy of the set of matched elements.
   * 
   * Note that this requires an override because Scala.Object declares a clone() method which
   * is entirely unrelated.
   */
  override def clone():JQuery = js.native
  def clone(withDataAndEvents:Boolean):JQuery = js.native
  def clone(withDataAndEvents:Boolean, deepWithDataAndEvents:Boolean):JQuery = js.native
  
  /**
   * Get the computed style properties for the first element in the set of matched elements.
   */
  def css(propertyName:String):String = js.native
  def css(propertyNames:Array[String]):js.Dictionary[String] = js.native
  def css(propertyName:String, value:String):JQuery = js.native
  def css(propertyName:String, value:Int):JQuery = js.native
  def css(properties:js.Dictionary[js.Any]):JQuery = js.native
  
  /**
   * Store arbitrary data associated with the matched elements.
   * 
   * undefined is not recognised as a data value. Calls such as .data( "name", undefined )
   * will return the corresponding data for "name", and is therefore the same as .data( "name" ).
   */
  def data(key: String, value: js.Any): JQuery = js.native
  def data(obj: js.Dictionary[js.Any]): JQuery = js.native
  /**
   * Return the value at the named data store for the first element in the jQuery collection, 
   * as set by data(name, value) or by an HTML5 data-* attribute.
   */
  def data(key: String): js.Any = js.native
  /**
   * Calling .data() with no parameters retrieves all of the values as a JavaScript object. 
   * This object can be safely cached in a variable as long as a new object is not set with .data(obj). 
   * Using the object directly to get or set values is faster than making individual calls to .data() 
   * to get or set each value.
   */
  def data(): js.Dictionary[js.Any] = js.native
  
  /**
   * Set a timer to delay execution of subsequent items in the queue.
   */
  def delay(duration:Int):JQuery = js.native
  def delay(duration:Int, queueName:String):JQuery = js.native
  
  /**
   * Remove the set of matched elements from the DOM.
   */
  def detach():JQuery = js.native
  def detach(selector:String):JQuery = js.native
  
  /**
   * Iterate over a jQuery object, executing a function for each matched element.
   * 
   * Note that we do not bother with the full jQuery signature, since the "element" parameter
   * simply matches "this".
   * 
   * You can stop the loop from within the callback function by returning false. Otherwise, the return
   * value is irrelevant.
   */
  def each(func:js.ThisFunction0[Element, Any]):JQuery = js.native
  def each(func:js.ThisFunction1[Element, Int, Any]):JQuery = js.native
  
  /**
   * Remove all child nodes of the set of matched elements from the DOM.
   */
  def empty():JQuery = js.native
  
  /**
   * Reduce the set of matched elements to those that match the selector or pass the function's test.
   */
  def filter(selector:String):JQuery = js.native
  def filter(func:js.ThisFunction0[Element, Boolean]):JQuery = js.native
  def filter(func:js.ThisFunction1[Element, Int, Boolean]):JQuery = js.native
  def filter(elements:Element*):JQuery = js.native
  def filter(selector:JQuery):JQuery = js.native
  
  /**
   * Get the descendants of each element in the current set of matched elements, filtered by a selector, jQuery object, or element.
   */
  def find(selector:String):JQuery = js.native
  def find(selector:Element):JQuery = js.native
  def find(selector:JQuery):JQuery = js.native
  
  /**
   * Reduce the set of matched elements to the first in the set.
   */
  def first():JQuery = js.native
  
  /**
   * Bind an event handler to the "focus" JavaScript event, or trigger that event on an element.
   */
  def focus():JQuery = js.native
  def focus(func:js.ThisFunction1[Element, JQueryEventObject, Any]):JQuery = js.native
  def focus(eventData:Any, func:js.ThisFunction1[Element, JQueryEventObject, Any]):JQuery = js.native

  /**
   * Retrieve one of the elements matched by the jQuery object.
   * 
   * If the value of index is out of bounds - less than the negative number of elements or equal to 
   * or greater than the number of elements - it returns undefined.
   */
  def get(index:Int):UndefOr[Element] = js.native
  /**
   * Retrieve the elements matched by the jQuery object.
   */
  def get():js.Array[_] = js.native
  
  /**
   * Determine whether any of the matched elements are assigned the given class.
   */
  def hasClass(className:String):Boolean = js.native
  
  /**
   * Get the current computed height for the first element in the set of matched elements.
   */
  def height():Double = js.native
  /**
   * Set the CSS height of every matched element.
   */
  def height(value:Double):JQuery = js.native
  def height(value:String):JQuery = js.native
  
  /**
   * Hide the matched elements.
   */
  def hide():JQuery = js.native
  def hide(duration:String):JQuery = js.native
  def hide(duration:String, complete:js.Function):JQuery = js.native
  def hide(duration:String, easing:String):JQuery = js.native
  def hide(duration:String, easing:String, complete:js.Function):JQuery = js.native
  def hide(duration:Int):JQuery = js.native
  def hide(duration:Int, complete:js.Function):JQuery = js.native
  def hide(duration:Int, easing:String):JQuery = js.native
  def hide(duration:Int, easing:String, complete:js.Function):JQuery = js.native
  // TODO: add the complex version of hide(), with a Builder to construct the Options.
  
  /**
   * Search for a given element from among the matched elements.
   */
  def index():Int = js.native
  def index(selector:String):Int = js.native
  def index(selector:Element):Int = js.native
  def index(selector:JQuery):Int = js.native
  
  /**
   * Insert every element in the set of matched elements before the target.
   */
  def insertBefore(selector:String):JQuery = js.native
  def insertBefore(selector:Element):JQuery = js.native
  def insertBefore(selector:JQuery):JQuery = js.native
  
  /**
   * Check the current matched set of elements against a selector, element,
   * or jQuery object and return true if at least one of these elements matches the given arguments.
   */
  def is(selector:Selector):Boolean = js.native
  /**
   * Note that this overload doesn't precisely match the jQuery documentation; we
   * elide the redundant Element param, since you have Element as the this parameter.
   */
  def is(func:js.ThisFunction1[Element, Int, Boolean]):Boolean = js.native
  
  /**
   * Bind an event handler to the "keydown" JavaScript event, or trigger that event on an element.
   */
  def keydown(handler:js.Function1[JQueryEventObject, Any]):JQuery = js.native
  def keydown(eventData:Any, handler:js.Function1[JQueryEventObject, Any]):JQuery = js.native
  def keydown():JQuery = js.native
  
  /**
   * Bind an event handler to the "keypress" JavaScript event, or trigger that event on an element.
   */
  def keypress(handler:js.Function1[JQueryEventObject, Any]):JQuery = js.native
  def keypress(eventData:Any, handler:js.Function1[JQueryEventObject, Any]):JQuery = js.native
  def keypress():JQuery = js.native
  
  /**
   * Bind an event handler to the "keyup" JavaScript event, or trigger that event on an element.
   */
  def keyup(handler:js.Function1[JQueryEventObject, Any]):JQuery = js.native
  def keyup(eventData:Any, handler:js.Function1[JQueryEventObject, Any]):JQuery = js.native
  def keyup():JQuery = js.native
  
  /**
   * The number of elements in the jQuery object.
   */
  def length:Int = js.native
  
  /**
   * Pass each element in the current matched set through a function, producing a new jQuery object
   * containing the return values.
   * 
   * For Scala code, it is often more convenient to use the mapElems() extension function.
   * 
   * Within the callback function, this refers to the current DOM element for each iteration. The function
   * can return an individual data item or an array of data items to be inserted into the resulting set.
   * 
   * If a js.Array is returned, the elements inside the array are inserted into the set.
   * If the function returns null or undefined, no element will be inserted. (Note the implication: this
   * doesn't quite match the usual Scala semantics of map() -- there is a flatten component as well.) 
   */
  def map(func:js.ThisFunction0[Element, Any]):JQuery = js.native
  def map(func:js.ThisFunction1[Element, Int, Any]):JQuery = js.native

  /**
   * Remove an event handler.
   */
  def off(events: String, selector: String, handler: js.ThisFunction1[Element, JQueryEventObject, Any]): JQuery = js.native
  def off(events: String, selector: String): JQuery = js.native
  def off(events: String): JQuery = js.native
  def off(): JQuery = js.native
  def off(eventsMap: js.Dictionary[js.ThisFunction1[Element, JQueryEventObject, Any]], selector: String): JQuery = js.native
  def off(eventsMap: js.Dictionary[js.ThisFunction1[Element, JQueryEventObject, Any]]): JQuery = js.native
  def off(event:JQueryEventObject):JQuery = js.native
  
  /**
   * Get the current coordinates of the first element in the set of matched elements, relative to the document.
   */
  def offset():JQueryPosition = js.native
  
  /**
   * Attach an event handler function for one or more events to the selected elements.
   */
  def on(events:String, selector: String, data: Any, handler: js.ThisFunction1[Element, JQueryEventObject, Any]): JQuery = js.native
  def on(events:String, handler: js.ThisFunction1[Element, JQueryEventObject, Any]): JQuery = js.native
  def on(events:String, handler: js.ThisFunction0[Element, Any]):JQuery = js.native
  def on(events:String, handler: js.Function1[JQueryEventObject, Any]): JQuery = js.native
  /**
   * Attach an event handler function for one or more events to the selected elements.
   * 
   * This version of the signature allows you to pass in "false" as the handler. This is kind of
   * magical in jQuery -- it is shorthand for a function that just does "return false", which
   * stops propagation on the event. Note that true is *not* a legal value, only false.
   */
  def on(events: String, selector: String, data: Any, turnOff:Boolean): JQuery = js.native
  def on(events: String, selector: String, data: Any): JQuery = js.native
  def on(events: String, selector: String): JQuery = js.native
  def on(events: String, turnOff:Boolean): JQuery = js.native
  def on(events: String): JQuery = js.native
  def on(eventsMap: js.Dictionary[js.ThisFunction1[Element, JQueryEventObject, Any]], selector: String, data: Any): JQuery = js.native
  def on(eventsMap: js.Dictionary[js.ThisFunction1[Element, JQueryEventObject, Any]], selector: String): JQuery = js.native
  def on(eventsMap: js.Dictionary[js.ThisFunction1[Element, JQueryEventObject, Any]]): JQuery = js.native

  /**
   * Get the parent of each element in the current set of matched elements, optionally filtered by a selector.
   */
  def parent(selector: String): JQuery = js.native
  def parent(): JQuery = js.native
  
  /**
   * Get the ancestors of each element in the current set of matched elements, optionally filtered by a selector.
   */
  def parents(selector:String):JQuery = js.native
  def parents():JQuery = js.native
  
  /**
   * Insert content, specified by the parameters, to the beginning of each element in the set of matched elements.
   */
  def prepend(contents:Selector*):JQuery = js.native
  def prepend(func:js.ThisFunction2[Element, Int, String, Selector]):JQuery = js.native
  
  /**
   * Get the value of a property for the first element in the set of matched elements.
   */
  def prop(propertyName:String):Any = js.native
  /**
   * Set one or more properties for the set of matched elements.
   */
  def prop(propertyName:String, value:js.Any):JQuery = js.native
  def prop(properties:js.Dictionary[js.Any]):JQuery = js.native
  def prop(propertyName:String, func:js.ThisFunction2[Element, Int, Any, js.Any]):JQuery = js.native
  
  /**
   * Remove the set of matched elements from the DOM.
   */
  def remove():JQuery = js.native
  def remove(childSelector:String):JQuery = js.native
  
  /**
   * Remove a single class, multiple classes, or all classes from each element in the set of matched elements.
   */
  def removeClass():JQuery = js.native
  def removeClass(classNames:String):JQuery = js.native
  def removeClass(func:js.ThisFunction2[Element, Int, String, String]):JQuery = js.native
  
  /**
   * Replace each element in the set of matched elements with the provided new content and return the set of elements that was removed.
   */
  def replaceWith(content:String):JQuery = js.native
  def replaceWith(content:Element):JQuery = js.native
  def replaceWith(content:JQuery):JQuery = js.native
  def replaceWith(content:Array[Element]):JQuery = js.native
  
  /**
   * Get the current vertical position of the scroll bar for the first element in the set of
   * matched elements or set the vertical position of the scroll bar for every matched element.
   */
  def scrollTop():Int = js.native
  /**
   * Set the current vertical position of the scroll bar for each of the set of matched elements.
   * 
   * Note that this intentionally takes Double -- while you usually want to set it to an Int, there
   * are occasions when being able to take a Double (that is, a full JS Number) is convenient in code.
   */
  def scrollTop(value:Double):JQuery = js.native
  
  /**
   * Hide the matched elements.
   */
  def show():JQuery = js.native
  def show(duration:String):JQuery = js.native
  def show(duration:String, complete:js.Function):JQuery = js.native
  def show(duration:String, easing:String):JQuery = js.native
  def show(duration:String, easing:String, complete:js.Function):JQuery = js.native
  def show(duration:Int):JQuery = js.native
  def show(duration:Int, complete:js.Function):JQuery = js.native
  def show(duration:Int, easing:String):JQuery = js.native
  def show(duration:Int, easing:String, complete:js.Function):JQuery = js.native
  // TODO: add the complex version of show(), with a Builder to construct the Options.
  
  /**
   * Display the matched elements with a sliding motion.
   */
  def slideDown():JQuery = js.native
  def slideDown(duration:String):JQuery = js.native
  def slideDown(duration:Int):JQuery = js.native
  
  /**
   * Hide the matched elements with a sliding motion.
   */
  def slideUp():JQuery = js.native
  def slideUp(duration:String):JQuery = js.native
  def slideUp(duration:Int):JQuery = js.native
  
  /**
   * Get the combined text contents of each element in the set of matched elements, including their descendants.
   */
  def text():String = js.native
  /**
   * Set the content of each element in the set of matched elements to the specified text.
   */
  def text(t:String):JQuery = js.native
  // TBD: the JQ docs don't say that this is a ThisFunction. Is it?
  def text(func:js.Function2[Int, String, String]):JQuery = js.native
  
  /**
   * Retrieve all the elements contained in the jQuery set, as an array.
   */
  def toArray():js.Array[_] = js.native
  
  /**
   * Execute all handlers and behaviors attached to the matched elements for the given event type.
   */
  def trigger(eventType:String):JQuery = js.native
  def trigger(event:JQueryEventObject):JQuery = js.native
  
  /**
   * Get the value of this JQuery.
   * 
   * "value" is highly context-dependent. The signature is loose because it can return a
   * String, a Number (?) or an Array, depending on circumstances. See the extension methods
   * in JQueryExtensions for more strongly-typed versions that you can use when you expect
   * a specific return type.
   */
  def `val`(): js.Dynamic = js.native
  def `val`(value: js.Array[String]): JQuery = js.native
  def `val`(value: String): JQuery = js.native
  def `val`(func: js.Function2[Int, String, String]): JQuery = js.native
  @JSName("val") def value(): js.Dynamic = js.native
  @JSName("val") def value(value: js.Array[String]): JQuery = js.native
  @JSName("val") def value(value: String): JQuery = js.native
  @JSName("val") def value(func: js.Function2[Int, String, String]): JQuery = js.native
  
  
  /**
   * Get the current computed width for the first element in the set of matched elements.
   */
  def width():Double = js.native
  /**
   * Set the CSS width of every matched element.
   */
  def width(value:Double):JQuery = js.native
  def width(value:String):JQuery = js.native
}

/**
 * Returned by offset() and position().
 * 
 * Note that the values in here are intentionally not Integers. From the JQuery docs:
 * 
 * "The number returned by dimensions-related APIs, including .offset(), may be fractional in some
 *  cases. Code should not assume it is an integer. Also, dimensions may be incorrect when the page
 *  is zoomed by the user; browsers do not expose an API to detect this condition."
 */
trait JQueryPosition extends js.Object {
  def left:Double = js.native
  def top:Double = js.native
}