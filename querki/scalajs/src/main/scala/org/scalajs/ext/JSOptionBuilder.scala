package org.scalajs.ext

import scala.scalajs.js

trait JSOptionBuilder[T <: js.Object] {
  /**
   * This is a dictionary of option values. It is usually *very* heterogeneous,
   * mixing everything from Ints to Functions. So it needs to be js.Any.
   */
  val dict:js.Dictionary[Any] = js.Dictionary.empty
  
  def jsOpt(name:String, opt:Any):this.type = {
    dict(name) = opt
    this
  }
  
  def result = dict.asInstanceOf[T]
  
  override def toString = {
    s"""{\n${dict.keys.map{ key => s"  $key = ${dict(key).toString}"}.mkString("\n")}\n}"""
  }
}
