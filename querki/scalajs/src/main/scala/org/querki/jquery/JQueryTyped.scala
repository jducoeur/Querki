package org.querki.jquery

import scala.scalajs.js
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
