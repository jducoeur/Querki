package org.scalajs.ext

import scala.scalajs.js

import js.JSConverters._

abstract class JSOptionBuilder[T <: js.Object, B <: JSOptionBuilder[T, _]](copy:OptMap => B) {
  /**
   * This is a dictionary of option values. It is usually *very* heterogeneous,
   * mixing everything from Ints to Functions. So it needs to be js.Any.
   */
  def dict:OptMap
  
  def jsOpt(name:String, opt:Any):B = {
    copy(dict + (name -> opt))
  }
  
  def result = {
    dict.toJSDictionary.asInstanceOf[T]
  }
  
  override def toString = {
    s"""{\n${dict.keys.map{ key => s"  $key = ${dict(key).toString}"}.mkString("\n")}\n}"""
  }
}
