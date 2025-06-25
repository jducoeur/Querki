package org.querki.squery

import org.scalajs.dom
import org.scalajs.dom.html._
import org.scalajs.dom.StyleSheet

/**
 * Very simple typeclass around the concept of enabled/disabled.
 */
trait Disableable[A] {
  def isDisabled(a: A): Boolean
  def isEnabled(a: A): Boolean = !isDisabled(a)
}

object Disableable {

  implicit val DisableableElement = new Disableable[dom.Element] {

    def isDisabled(e: dom.Element) = {
      e match {
        case button: Button     => button.disabled
        case fieldSet: FieldSet => fieldSet.disabled
        case input: Input       => input.disabled
        case optGroup: OptGroup => optGroup.disabled
        case option: Option     => option.disabled
        case select: Select     => select.disabled
        case textArea: TextArea => textArea.disabled
        case _                  => false
      }
    }
  }

  implicit val DisableableStylesheet = new Disableable[StyleSheet] {
    def isDisabled(s: StyleSheet) = s.disabled
  }

  implicit class DisableableBuilder[T : Disableable](t: T) {
    def isDisabled: Boolean = implicitly[Disableable[T]].isDisabled(t)
    def isEnabled: Boolean = implicitly[Disableable[T]].isEnabled(t)
  }
}
