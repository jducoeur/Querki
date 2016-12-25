package org.querki.squery

import org.scalajs.dom
import dom.Element
import dom.html.{Element => HTMLElement}

trait AttrFunctions[A] {
  def isDisabled(a:A):Boolean
  def isEnabled(a:A):Boolean = !isDisabled(a)
}

object AttrFunctions {
  implicit val ElementAttrFunctions = new AttrFunctions[Element] {
    def isDisabled(e:Element) = {
      e match {
        case html:HTMLElement => html.disabled.getOrElse(false)
        case _ => false
      }
    }
  }
  
  implicit class ElementAttrEasy(e:Element) {
    def isDisabled:Boolean = ElementAttrFunctions.isDisabled(e)
    def isEnabled:Boolean = ElementAttrFunctions.isEnabled(e)
  }
}
