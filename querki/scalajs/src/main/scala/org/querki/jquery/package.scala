package org.querki

import scala.scalajs.js
import org.scalajs.dom
import dom.Element

package object jquery {
  /**
   * The main entry point into jQuery. We alias it to $, to match jQuery idiom.
   */
  val $ = JQueryStatic
  
  /**
   * A shorter alias for JQuery events, just to reduce keystrokes.
   */
  type JQEvt = JQueryEventObject
  
  implicit def builder2DialogOptions(builder:JQueryEventObjectBuilder) = builder._result

  implicit def jQuery2Ext(jq:JQuery):JQueryExtensions = new JQueryExtensions(jq)
  
  implicit def strToSelector(v:String):Selector = v.asInstanceOf[Selector]
  implicit def elementToSelector(v:Element):Selector = v.asInstanceOf[Selector]
  implicit def arrayToSelector(v:Array[Element]):Selector = v.asInstanceOf[Selector]
}
