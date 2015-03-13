package org.querki.jquery

import scala.scalajs.js
import js.UndefOr
import org.scalajs.dom._

/**
 * These are strongly-typed signatures for polymorphic JQuery classes. They each call
 * internal classes in the underlying JQuery facade which are loosely-typed.
 * 
 * You should basically consider this class to be complementary to JQuery, and you should
 * pretty much always import it when trying to work with JQuery objects. It is implicitly
 * converted from JQuery, and provides the strongly-typed facade over the weakly-typed
 * internal facades in that.
 */
class JQueryTyped(jq:JQuery) {
  /**
   * Insert content, specified by the parameter, after each element in the set of matched elements.
   */
  def after(content:ElementDesc):JQuery = jq.afterInternal(toJsAny(content))
  def after(func:js.ThisFunction0[Element, ElementDesc]):JQuery = jq.afterInternal(func)
  def after(func:js.ThisFunction1[Element, Int, ElementDesc]):JQuery =  jq.afterInternal(func)
  
  /**
   * Insert every element in the set of matched elements to the end of the target.
   */
  def appendTo(target:ElementDesc):JQuery = jq.appendToInternal(toJsAny(target))
  
  /**
   * Get the value of an attribute for the first element in the set of matched elements.
   * 
   * Note that this returns UndefOr -- it is entirely legal for this to return undefined if
   * the attribute is not present, and that causes things to crash if it is not UndefOr.
   */
  def attr(attributeName:String):UndefOr[String] = jq.attrInternal(attributeName)
  def attr(attributes:js.Dictionary[String]):JQuery = jq.attrInternal(attributes)
  /**
   * Set an attribute for the set of matched elements.
   */
  def attr(attributeName:String, v:AttrVal):JQuery = jq.attrInternal(attributeName, toJsAny(v))
  def attr(attributeName:String, func:js.ThisFunction2[Element, Int, String, AttrVal]):JQuery = jq.attrInternal(attributeName, func)
  
  /**
   * Reduce the set of matched elements to those that match the selector or pass the function's test.
   */
  def filter(selector:Selector):JQuery = jq.filterInternal(toJsAny(selector))
  def filter(func:js.ThisFunction0[Element, Boolean]):JQuery = jq.filterInternal(func)
  def filter(func:js.ThisFunction1[Element, Int, Boolean]):JQuery = jq.filterInternal(func)
  
  /**
   * Get the descendants of each element in the current set of matched elements, filtered by a selector, jQuery object, or element.
   */
  def find(selector:Selector):JQuery = jq.findInternal(toJsAny(selector))
  
  /**
   * Search for a given element from among the matched elements.
   */
  def index():Int = jq.indexInternal()
  def index(selector:ElementDesc):Int = jq.indexInternal(toJsAny(selector))
  
  /**
   * Insert every element in the set of matched elements before the target.
   */
  def insertBefore(target:ElementDesc):JQuery = jq.insertBeforeInternal(toJsAny(target))
  
  /**
   * Check the current matched set of elements against a selector, element,
   * or jQuery object and return true if at least one of these elements matches the given arguments.
   */
  def is(selector:Selector):Boolean = jq.isInternal(toJsAny(selector))
  /**
   * Note that this overload doesn't precisely match the jQuery documentation; we
   * elide the redundant Element param, since you have Element as the this parameter.
   */
  def is(func:js.ThisFunction1[Element, Int, Boolean]):Boolean = jq.isInternal(func)
  
  /**
   * Insert content, specified by the parameters, to the beginning of each element in the set of matched elements.
   */
  def prepend(contents:Selector*):JQuery = jq.prependInternal(contents.map(toJsAny(_)):_*)
  def prepend(func:js.ThisFunction2[Element, Int, String, Selector]):JQuery = jq.prependInternal(func)
  
  /**
   * Replace each element in the set of matched elements with the provided new content and return the set of elements that was removed.
   */
  def replaceWith(content:ElementDesc):JQuery = jq.replaceWithInternal(toJsAny(content))
  def replaceWith(func:js.ThisFunction0[Element, ElementDesc]):JQuery =  jq.replaceWithInternal(func)
}
