package org.querki.jquery

import scala.scalajs.js
import org.scalajs.dom.Element
  
/**
 * These are extensions to jQuery -- useful higher-level functions, which mostly tighten up the types.
 * 
 * Basically, when you want to express a common idiom, especially one that is particularly helpful
 * in the Scala environment, it belongs here. But this isn't for complex high-level logic, just for
 * stuff that makes JQuery easier to use in Scala.
 */
class JQueryExtensions(jq:JQuery) {
  /**
   * Fetch the name attribute, which *must* be set.
   * 
   * This is nothing more than a convenience wrapper around JQuery.attr().
   * 
   * TBD: is there a better naming convention for this? I'd really like attr!(), but that syntax
   * doesn't work, sadly.
   */
  def Attr(attributeName:String):String = jq.attr(attributeName).get
  
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
}
