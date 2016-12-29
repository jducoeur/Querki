package org.querki.squery

import org.scalajs.dom
import dom.Element
import dom.html.{Element => HTMLElement}

import Disableable._

/**
 * This typeclass represents the ability to put the browser "focus" on something on-screen.
 */
trait Focusable[A] {
  /**
   * Returns true iff this can receive focus.
   */
  def canFocus(a:A):Boolean
  
  /**
   * Side-effecting: puts the input focus on a.
   */
  def focus(a:A):Unit
}

object Focusable {
  /**
   * Implements focus functions for Elements.
   * 
   * TODO: Note that this currently applies to *all* Elements, not just HTMLElements.
   * This is arguably wrong, but we need to tweak other parts of the sQuery API to make
   * it practical to restrict this.
   */
  implicit val ElementFormEvents = new Focusable[Element] {
    /**
     * Returns true iff this Element can *currently* receive focus.
     * 
     * TODO: this is a step in the right direction, but not nearly good enough yet.
     * See https://api.jqueryui.com/focusable-selector/ for some discussion of the
     * issues. There's a lot of subtlety in getting this right.
     */
    def canFocus(e:Element):Boolean = {
      e.tagName match {
        case "A" | "BUTTON" | "INPUT" | "SELECT" | "TEXTAREA" => {
          e.isEnabled
        }
        case _ => false
      }      
    }
    
    /**
     * Put the browser focus on this, if it is an HTMLElement. Note that this does not
     * check whether this is a valid focus target; if not, nothing will happen. Use
     * canFocus() if you want to be smart about this.
     */
    def focus(e:Element):Unit = {
      e match {
        case html:HTMLElement => {
          html.focus()
        }
        case _ =>
      }
    }
  }
  
  implicit class FocusableBuilder[T : Focusable](t:T) {
    def canFocus:Boolean = implicitly[Focusable[T]].canFocus(t)
    def focus():Unit = implicitly[Focusable[T]].focus(t)
  }
}
