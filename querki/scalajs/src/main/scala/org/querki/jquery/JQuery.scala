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
 * sometimes it means all of the possible types.
 * 
 * TODO: as of 0.5.5, this doesn't compile, because js.Any was sealed. This is fixed in 0.6.
 * 
 * TODO: many of the signatures below should be tweaked to use Selector, once we've proved
 * that works!
 */
sealed trait Selector extends js.Any

/**
 * This is a pseudo-union type. From the Scala POV, it's simply an anonymous trait, but we
 * use it in jQuery signatures, for the fairly common scenario where a signature takes
 * either a String or an Int, to reduce multiplicative explosion of overloads.
 */
sealed trait StringOrInt extends js.Any

/**
 * This pseudo-union type represents the various types that can be returned from replaceWith().
 * Note that there are a bunch of implicit defs that describe conversions to this.
 */
sealed trait ElementDesc extends js.Any

/**
 * This pseudo-union type represents the types that can be passed into attr().
 */
sealed trait AttrVal extends js.Any

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
 * Many parameters are polymorphic. When there is a common pattern to that polymorphism -- that is, when
 * the same overloads are repeatedly used in many functions -- I've pulled that out into a pseudo-union
 * trait. (Notably Selector, but not limited to that.) We shouldn't go overboard on that, but this seems
 * a practical approach in many cases.
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
  def addClass(classNames:String):JQuery = ???
  def addClass(func:js.ThisFunction2[Element, Int, String, String]):JQuery = ???
  
  /**
   * Insert content, specified by the parameter, after each element in the set of matched elements.
   */
  def after(content:ElementDesc):JQuery = ???
  def after(func:js.ThisFunction0[Element, ElementDesc]):JQuery = ???
  def after(func:js.ThisFunction1[Element, Int, ElementDesc]):JQuery = ???
  
  /**
   * Insert content, specified by the parameter, to the end of each element in the set of matched elements.
   * 
   * The content can be any number of elements, each one an HTML String *or* an Element *or* an Array
   * *or* a jQuery. There is no clean way to build a strongly-typed Scala signature for this.
   */
  def append(content:js.Any*):JQuery = ???
  def append(func:js.ThisFunction2[Element, Int, String, js.Any]):JQuery = ???
  
  /**
   * Insert every element in the set of matched elements to the end of the target.
   */
  def appendTo(target:String):JQuery = ???
  def appendTo(target:Element):JQuery = ???
  def appendTo(target:JQuery):JQuery = ???
  
  /**
   * Get the value of an attribute for the first element in the set of matched elements.
   * 
   * Note that this returns UndefOr -- it is entirely legal for this to return undefined if
   * the attribute is not present, and that causes things to crash if it is not UndefOr.
   */
  def attr(attributeName:String):UndefOr[String] = ???
  def attr(attributeName:String, v:AttrVal):JQuery = ???
  def attr(attributes:js.Dictionary[String]):JQuery = ???
  def attr(attributeName:String, func:js.ThisFunction2[Element, Int, String, AttrVal]):JQuery = ???
  
  /**
   * Bind an event handler to the "change" JavaScript event, or trigger that event on an element.
   * 
   * NOTE: the jQuery documentation is very fuzzy on this point, but implies in an example that "this" gets
   * set appropriately.
   */
  def change(handler:js.ThisFunction0[Element, Any]):JQuery = ???
  def change(handler:js.ThisFunction1[Element, JQueryEventObject, Any]):JQuery = ???
  def change(handler:js.Function1[JQueryEventObject, Any]):JQuery = ???
  def change(eventData:Any, handler:js.ThisFunction1[Element, JQueryEventObject, Any]):JQuery = ???
  def change():JQuery = ???
  
  /**
   * Get the children of each element in the set of matched elements, optionally filtered by a selector.
   */
  def children(selector:String):JQuery = ???
  def children():JQuery = ???
    
  /**
   * Bind an event handler to the "click" JavaScript event.
   * 
   * This is the simpler version, and usually what you want.
   */
  def click(func:js.ThisFunction0[Element, Any]):JQuery = ???
  def click(func:js.ThisFunction1[Element, JQueryEventObject, Any]):JQuery = ???
  def click(func:js.Function1[JQueryEventObject, Any]):JQuery = ???
  
  /**
   * Create a deep copy of the set of matched elements.
   * 
   * Note that this requires an override because Scala.Object declares a clone() method which
   * is entirely unrelated.
   */
  override def clone():JQuery = ???
  def clone(withDataAndEvents:Boolean):JQuery = ???
  def clone(withDataAndEvents:Boolean, deepWithDataAndEvents:Boolean):JQuery = ???
  
  /**
   * Get the computed style properties for the first element in the set of matched elements.
   */
  def css(propertyName:String):String = ???
  def css(propertyNames:Array[String]):js.Dictionary[String] = ???
  def css(propertyName:String, value:StringOrInt):JQuery = ???
  def css(propertyName:String, func:js.ThisFunction2[Element, Int, String, StringOrInt]):JQuery = ???
  def css(properties:js.Dictionary[StringOrInt]):JQuery = ???
  
  /**
   * Store arbitrary data associated with the matched elements.
   * 
   * undefined is not recognised as a data value. Calls such as .data( "name", undefined )
   * will return the corresponding data for "name", and is therefore the same as .data( "name" ).
   */
  def data(key: String, value: js.Any): JQuery = ???
  def data(obj: js.Dictionary[js.Any]): JQuery = ???
  /**
   * Return the value at the named data store for the first element in the jQuery collection, 
   * as set by data(name, value) or by an HTML5 data-* attribute.
   */
  def data(key: String): js.Any = ???
  /**
   * Calling .data() with no parameters retrieves all of the values as a JavaScript object. 
   * This object can be safely cached in a variable as long as a new object is not set with .data(obj). 
   * Using the object directly to get or set values is faster than making individual calls to .data() 
   * to get or set each value.
   */
  def data(): js.Dictionary[js.Any] = ???
  
  /**
   * Set a timer to delay execution of subsequent items in the queue.
   */
  def delay(duration:Int):JQuery = ???
  def delay(duration:Int, queueName:String):JQuery = ???
  
  /**
   * Remove the set of matched elements from the DOM.
   */
  def detach():JQuery = ???
  def detach(selector:String):JQuery = ???
  
  /**
   * Iterate over a jQuery object, executing a function for each matched element.
   * 
   * Note that we do not bother with the full jQuery signature, since the "element" parameter
   * simply matches "this".
   * 
   * You can stop the loop from within the callback function by returning false. Otherwise, the return
   * value is irrelevant.
   */
  def each(func:js.ThisFunction0[Element, Any]):JQuery = ???
  def each(func:js.ThisFunction1[Element, Int, Any]):JQuery = ???
  
  /**
   * Remove all child nodes of the set of matched elements from the DOM.
   */
  def empty():JQuery = ???
  
  /**
   * Reduce the set of matched elements to those that match the selector or pass the function's test.
   */
  def filter(selector:String):JQuery = ???
  def filter(func:js.ThisFunction2[Element, Int, Element, Boolean]):JQuery = ???
  def filter(elements:Element*):JQuery = ???
  def filter(selector:JQuery):JQuery = ???
  
  /**
   * Get the descendants of each element in the current set of matched elements, filtered by a selector, jQuery object, or element.
   */
  def find(selector:String):JQuery = ???
  def find(selector:Element):JQuery = ???
  def find(selector:JQuery):JQuery = ???
  
  /**
   * Reduce the set of matched elements to the first in the set.
   */
  def first():JQuery = ???
  
  /**
   * Bind an event handler to the "focus" JavaScript event, or trigger that event on an element.
   */
  def focus():JQuery = ???
  def focus(func:js.ThisFunction1[Element, JQueryEventObject, Any]):JQuery = ???
  def focus(eventData:Any, func:js.ThisFunction1[Element, JQueryEventObject, Any]):JQuery = ???

  /**
   * Retrieve one of the elements matched by the jQuery object.
   * 
   * If the value of index is out of bounds - less than the negative number of elements or equal to 
   * or greater than the number of elements - it returns undefined.
   */
  def get(index:Int):UndefOr[Element] = ???
  /**
   * Retrieve the elements matched by the jQuery object.
   */
  def get():js.Array[_] = ???
  
  /**
   * Determine whether any of the matched elements are assigned the given class.
   */
  def hasClass(className:String):Boolean = ???
  
  /**
   * Get the current computed height for the first element in the set of matched elements.
   */
  def height():Double = ???
  /**
   * Set the CSS height of every matched element.
   */
  def height(value:Double):JQuery = ???
  def height(value:String):JQuery = ???
  def height(func:js.ThisFunction2[Element, Int, Int, StringOrInt]):JQuery = ???
  
  /**
   * Hide the matched elements.
   */
  def hide():JQuery = ???
  def hide(duration:StringOrInt):JQuery = ???
  def hide(duration:StringOrInt, complete:js.Function):JQuery = ???
  def hide(duration:StringOrInt, easing:String):JQuery = ???
  def hide(duration:StringOrInt, easing:String, complete:js.Function):JQuery = ???
  // TODO: add the complex version of hide(), with a Builder to construct the Options.
  
  /**
   * Search for a given element from among the matched elements.
   */
  def index():Int = ???
  def index(selector:String):Int = ???
  def index(selector:Element):Int = ???
  def index(selector:JQuery):Int = ???
  
  /**
   * Insert every element in the set of matched elements before the target.
   */
  def insertBefore(selector:String):JQuery = ???
  def insertBefore(selector:Element):JQuery = ???
  def insertBefore(selector:JQuery):JQuery = ???
  
  /**
   * Check the current matched set of elements against a selector, element,
   * or jQuery object and return true if at least one of these elements matches the given arguments.
   */
  def is(selector:Selector):Boolean = ???
  /**
   * Note that this overload doesn't precisely match the jQuery documentation; we
   * elide the redundant Element param, since you have Element as the this parameter.
   */
  def is(func:js.ThisFunction1[Element, Int, Boolean]):Boolean = ???
  
  /**
   * Bind an event handler to the "keydown" JavaScript event, or trigger that event on an element.
   */
  def keydown(handler:js.Function1[JQueryEventObject, Any]):JQuery = ???
  def keydown(eventData:Any, handler:js.Function1[JQueryEventObject, Any]):JQuery = ???
  def keydown():JQuery = ???
  
  /**
   * Bind an event handler to the "keypress" JavaScript event, or trigger that event on an element.
   */
  def keypress(handler:js.Function1[JQueryEventObject, Any]):JQuery = ???
  def keypress(eventData:Any, handler:js.Function1[JQueryEventObject, Any]):JQuery = ???
  def keypress():JQuery = ???
  
  /**
   * Bind an event handler to the "keyup" JavaScript event, or trigger that event on an element.
   */
  def keyup(handler:js.Function1[JQueryEventObject, Any]):JQuery = ???
  def keyup(eventData:Any, handler:js.Function1[JQueryEventObject, Any]):JQuery = ???
  def keyup():JQuery = ???
  
  /**
   * The number of elements in the jQuery object.
   */
  def length:Int = ???
  
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
  def map(func:js.ThisFunction0[Element, Any]):JQuery = ???
  def map(func:js.ThisFunction1[Element, Int, Any]):JQuery = ???

  /**
   * Remove an event handler.
   */
  def off(events: String, selector: String, handler: js.ThisFunction1[Element, JQueryEventObject, Any]): JQuery = ???
  def off(events: String, selector: String): JQuery = ???
  def off(events: String): JQuery = ???
  def off(): JQuery = ???
  def off(eventsMap: js.Dictionary[js.ThisFunction1[Element, JQueryEventObject, Any]], selector: String): JQuery = ???
  def off(eventsMap: js.Dictionary[js.ThisFunction1[Element, JQueryEventObject, Any]]): JQuery = ???
  def off(event:JQueryEventObject):JQuery = ???
  
  /**
   * Attach an event handler function for one or more events to the selected elements.
   */
  def on(events:String, selector: String, data: Any, handler: js.ThisFunction1[Element, JQueryEventObject, Any]): JQuery = ???
  def on(events:String, handler: js.ThisFunction1[Element, JQueryEventObject, Any]): JQuery = ???
  def on(events:String, handler:js.ThisFunction0[Element, Any]):JQuery = ???
  def on(events:String, handler: js.Function1[JQueryEventObject, Any]): JQuery = ???
  /**
   * Attach an event handler function for one or more events to the selected elements.
   * 
   * This version of the signature allows you to pass in "false" as the handler. This is kind of
   * magical in jQuery -- it is shorthand for a function that just does "return false", which
   * stops propagation on the event. Note that true is *not* a legal value, only false.
   */
  def on(events: String, selector: String, data: Any, turnOff:Boolean): JQuery = ???
  def on(events: String, selector: String, data: Any): JQuery = ???
  def on(events: String, selector: String): JQuery = ???
  def on(events: String, turnOff:Boolean): JQuery = ???
  def on(events: String): JQuery = ???
  def on(eventsMap: js.Dictionary[js.ThisFunction1[Element, JQueryEventObject, Any]], selector: String, data: Any): JQuery = ???
  def on(eventsMap: js.Dictionary[js.ThisFunction1[Element, JQueryEventObject, Any]], selector: String): JQuery = ???
  def on(eventsMap: js.Dictionary[js.ThisFunction1[Element, JQueryEventObject, Any]]): JQuery = ???

  /**
   * Get the parent of each element in the current set of matched elements, optionally filtered by a selector.
   */
  def parent(selector: String): JQuery = ???
  def parent(): JQuery = ???
  
  /**
   * Get the ancestors of each element in the current set of matched elements, optionally filtered by a selector.
   */
  def parents(selector:String):JQuery = ???
  def parents():JQuery = ???
  
  /**
   * Get the value of a property for the first element in the set of matched elements.
   */
  def prop(propertyName:String):Any = ???
  /**
   * Set one or more properties for the set of matched elements.
   */
  def prop(propertyName:String, value:js.Any):JQuery = ???
  def prop(properties:js.Dictionary[js.Any]):JQuery = ???
  def prop(propertyName:String, func:js.ThisFunction2[Element, Int, Any, js.Any]):JQuery = ???
  
  /**
   * Remove the set of matched elements from the DOM.
   */
  def remove():JQuery = ???
  def remove(childSelector:String):JQuery = ???
  
  /**
   * Remove a single class, multiple classes, or all classes from each element in the set of matched elements.
   */
  def removeClass():JQuery = ???
  def removeClass(classNames:String):JQuery = ???
  def removeClass(func:js.ThisFunction2[Element, Int, String, String]):JQuery = ???
  
  /**
   * Replace each element in the set of matched elements with the provided new content and return the set of elements that was removed.
   * 
   * Note that this takes the ElementDesc pseudo-type. See the jquery package for the implicit defs that
   * convert to this.
   */
  def replaceWith(content:ElementDesc):JQuery = ???
  def replaceWith(func:js.ThisFunction0[Element, ElementDesc]):JQuery = ???
  
  /**
   * Get the current vertical position of the scroll bar for the first element in the set of
   * matched elements or set the vertical position of the scroll bar for every matched element.
   */
  def scrollTop():Int = ???
  def scrollTop(value:Int):JQuery = ???
  
  /**
   * Hide the matched elements.
   */
  def show():JQuery = ???
  def show(duration:StringOrInt):JQuery = ???
  def show(duration:StringOrInt, complete:js.Function):JQuery = ???
  def show(duration:StringOrInt, easing:String):JQuery = ???
  def show(duration:StringOrInt, easing:String, complete:js.Function):JQuery = ???
  // TODO: add the complex version of show(), with a Builder to construct the Options.
  
  /**
   * Display the matched elements with a sliding motion.
   */
  def slideDown():JQuery = ???
  def slideDown(duration:StringOrInt):JQuery = ???
  
  /**
   * Hide the matched elements with a sliding motion.
   */
  def slideUp():JQuery = ???
  def slideUp(duration:StringOrInt):JQuery = ???
  
  /**
   * Get the combined text contents of each element in the set of matched elements, including their descendants.
   */
  def text():String = ???
  /**
   * Set the content of each element in the set of matched elements to the specified text.
   */
  def text(t:String):JQuery = ???
  // TBD: the JQ docs don't say that this is a ThisFunction. Is it?
  def text(func:js.Function2[Int, String, String]):JQuery = ???
  
  /**
   * Retrieve all the elements contained in the jQuery set, as an array.
   */
  def toArray():js.Array[_] = ???
  
  /**
   * Execute all handlers and behaviors attached to the matched elements for the given event type.
   */
  def trigger(eventType:String):JQuery = ???
  def trigger(event:JQueryEventObject):JQuery = ???
  
  /**
   * Get the value of this JQuery.
   * 
   * "value" is highly context-dependent. The signature is loose because it can return a
   * String, a Number (?) or an Array, depending on circumstances. See the extension methods
   * in JQueryExtensions for more strongly-typed versions that you can use when you expect
   * a specific return type.
   */
  def `val`(): js.Dynamic = ???
  def `val`(value: js.Array[String]): JQuery = ???
  def `val`(value: String): JQuery = ???
  def `val`(func: js.Function2[Int, String, String]): JQuery = ???
  @JSName("val") def value(): js.Dynamic = ???
  @JSName("val") def value(value: js.Array[String]): JQuery = ???
  @JSName("val") def value(value: String): JQuery = ???
  @JSName("val") def value(func: js.Function2[Int, String, String]): JQuery = ???
  
  
  /**
   * Get the current computed width for the first element in the set of matched elements.
   */
  def width():Double = ???
  /**
   * Set the CSS width of every matched element.
   */
  def width(value:Double):JQuery = ???
  def width(value:String):JQuery = ???
  def width(func:js.ThisFunction2[Element, Int, Int, StringOrInt]):JQuery = ???
}
