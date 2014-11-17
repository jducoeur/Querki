package org.scalajs.ext

import scala.scalajs.js

trait JSOptionBuilder[T <: js.Object] {

  /**
   * This is a dictionary of option values. It is usually *very* heterogeneous,
   * mixing everything from Ints to Functions. So it needs to be js.Any.
   */
  val dict:js.Dictionary[Any] = js.Dictionary.empty
  
  def jsOption[U](name:String) = JSOption[T, U](this, name)
  
  def result = dict.asInstanceOf[T]
}

/**
 * The object that actually wraps around an option.
 * 
 * This is pulled out from JSOptionBuilder solely because (I suspect) it helps
 * reduce code size.
 */
private [ext] case class JSOption[T <: js.Object, U](builder:JSOptionBuilder[T], name:String) {
  def apply(opt:U) = {
    builder.dict(name) = opt
    builder
  }
}
