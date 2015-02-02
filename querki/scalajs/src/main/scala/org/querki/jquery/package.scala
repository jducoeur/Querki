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
   * A shorter alias for JQuery events, just to reduce keystrokes.
   */
  type JQEvt = JQueryEventObject
  
  implicit def builder2DialogOptions(builder:JQueryEventObjectBuilder) = builder._result

  implicit def jQuery2Ext(jq:JQuery):JQueryExtensions = new JQueryExtensions(jq)
  
  implicit def strToStringOrInt(s: String): StringOrInt = s.asInstanceOf[StringOrInt]
  implicit def intToStringOrInt(i: Int): StringOrInt = i.asInstanceOf[StringOrInt]
  
  implicit def strToAttrVal(v:String):AttrVal = v.asInstanceOf[AttrVal]
  implicit def intToAttrVal(v:Int):AttrVal = v.asInstanceOf[AttrVal]
  implicit def boolToAttrVal(v:Boolean):AttrVal = v.asInstanceOf[AttrVal]
  
  implicit def strToElementDesc(v:String):ElementDesc = v.asInstanceOf[ElementDesc]
  implicit def elementToElementDesc(v:Element):ElementDesc = v.asInstanceOf[ElementDesc]
  implicit def jqToElementDesc(v:JQuery):ElementDesc = v.asInstanceOf[ElementDesc]
  implicit def arrayToElementDesc(v:Array[Element]):ElementDesc = v.asInstanceOf[ElementDesc]
  
  implicit def strToSelector(v:String):Selector = v.asInstanceOf[Selector]
  implicit def elementToSelector(v:Element):Selector = v.asInstanceOf[Selector]
  implicit def arrayToSelector(v:Array[Element]):Selector = v.asInstanceOf[Selector]
}
