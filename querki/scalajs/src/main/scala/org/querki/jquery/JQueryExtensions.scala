package org.querki.jquery

import scala.scalajs.js
import org.scalajs.dom.Element
  
/**
 * These are extensions to jQuery -- useful higher-level functions, which mostly tighten up the types.
 * 
 * Basically, when you want to express a common idiom, especially one that is particularly helpful
 * in the Scala environment, it belongs here. But this isn't for complex high-level logic, just for
 * stuff that makes JQuery easier to use in Scala.
 * 
 * Within those constraints, pull requests are welcome for additional utility functions that seem
 * to be at the same level.
 */
class JQueryExtensions(jq:JQuery) {
  /**
   * Fetch this data value as a String.
   * 
   * You should only call this if you *know* with confidence that this data field is set,
   * and that the value is String; it will crash otherwise.
   */
  def dataString(name:String) = jq.data(name).asInstanceOf[String]
    
  /**
   * Wrap $.map in something more idiomatic and convenient for Scala
   * 
   * This applies the given function to each element in this JQuery object, and returns the
   * results. Note that, unlike JQuery.map(), this produces the unwrapped results, since that
   * is typically what you want in Scala code. 
   */ 
  def mapElems[T](func:Element => T):Seq[T] = {
    jq.map({ e:Element =>
      func(e)
    }:js.ThisFunction0[Element, Any]).toArray().toArray.asInstanceOf[Array[T]]
  }
  
  /**
   * The value of this Element; use this when it can only make sense as a String in context,
   * and when you are confident that the value is set.
   */
  def valueString = jq.value().asInstanceOf[String]
	
  /**
   * Execute the given code over each Element in the returned set. This is just convenience sugar
   * around $.each(), but is typically easier to use.
   */
  def foreach(func:Element => Unit):JQuery = {
    jq.each({ e:Element =>
      func(e)
    }:js.ThisFunction0[Element, Any])
    jq
  }
    
  /**
   * JQuery's native replaceWith is useful *if* you are planning on throwing away the node you're
   * replacing. But if you're going to want to restore it, it's bad because it *removes* the old
   * element from the DOM, losing its data and stuff. So this is a similar function, which
   * *detaches* the old element instead of removing it.
   */
  def detachReplaceWith(e:Element):JQuery = {
    $(e).insertBefore(jq)
    jq.detach()
  }
}
