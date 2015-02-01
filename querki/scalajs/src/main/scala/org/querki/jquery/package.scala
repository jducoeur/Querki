package org.querki

import scala.scalajs.js
import org.scalajs.dom
import dom.Element

package object jquery extends js.GlobalScope {
  val jQuery:JQueryStatic = ???
  
  /**
   * The main entry point into jQuery. We alias it to $, to match jQuery idiom.
   */
  val $ = jQuery
  
  /**
   * A shorter alias for JQuery events.
   */
  type JQE = JQueryEventObject
  
  implicit def builder2DialogOptions(builder:JQueryEventObjectBuilder) = builder._result

  implicit def jQuery2Ext(jq:JQuery):JQueryExtensions = new JQueryExtensions(jq)
  
  /**
   * This is a nasty trick to allow pseudo-union Types.
   */
  sealed trait StringOrInt
  implicit def strToStringOrInt(s: String): StringOrInt = s.asInstanceOf[StringOrInt]
  implicit def intToStringOrInt(i: Int): StringOrInt = i.asInstanceOf[StringOrInt]
  
  // TODO: add a similar "union trait" for Selector, and rewrite signatures to use that.
}
