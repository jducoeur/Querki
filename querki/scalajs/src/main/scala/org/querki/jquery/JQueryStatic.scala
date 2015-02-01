package org.querki.jquery

import scala.scalajs.js
import org.scalajs.dom
import dom.Element

trait JQueryStatic extends js.Object {
  def apply(selector: String, context: js.Any): JQuery = ???
  def apply(selector: String): JQuery = ???
  def apply(element: Element): JQuery = ???
  def apply(`object`: js.Any): JQuery = ???
  def apply(elementArray: js.Array[Element]): JQuery = ???
  def apply(`object`: JQuery): JQuery = ???
  def apply(func: js.Function): JQuery = ???
  def apply(): JQuery = ???
  
  var expr: js.Dynamic = ???
}
