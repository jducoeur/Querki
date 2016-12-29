package org.querki.squery

import org.scalajs.dom
import dom.Element
import dom.html.{Element => HTMLElement}

/**
 * Very simple typeclass around the concept of enabled/disabled.
 */
trait Disableable[A] {
  def isDisabled(a:A):Boolean
  def isEnabled(a:A):Boolean = !isDisabled(a)
}

object Disableable {
  implicit val DisableableElement = new Disableable[Element] {
    def isDisabled(e:Element) = {
      e match {
        case html:HTMLElement => html.disabled.getOrElse(false)
        case _ => false
      }
    }
  }
  
  implicit class DisableableBuilder[T : Disableable](t:T) {
    def isDisabled:Boolean = implicitly[Disableable[T]].isDisabled(t)
    def isEnabled:Boolean = implicitly[Disableable[T]].isEnabled(t)
  }
}
