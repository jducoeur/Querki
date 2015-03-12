package org.querki.jquery

import scala.scalajs.js
import js.annotation.JSName
import org.scalajs.dom
import dom.Element

@JSName("jQuery")
object JQueryStatic extends js.Object {
  def apply(selector: String, context: js.Any): JQuery = js.native
  def apply(selector: String): JQuery = js.native
  def apply(element: Element): JQuery = js.native
  def apply(`object`: js.Any): JQuery = js.native
  def apply(elementArray: js.Array[Element]): JQuery = js.native
  def apply(`object`: JQuery): JQuery = js.native
  def apply(func: js.Function): JQuery = js.native
  def apply(): JQuery = js.native
  
  var expr: js.Dynamic = js.native
}
