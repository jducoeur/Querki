package org.querki.squery

import org.scalajs.dom
import dom.Element
import dom.html.{Element => HTMLElement}

import AttrFunctions._

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
     * 
     * TBD: should this be restricted to HTMLElement in the first place? I can make a
     * very good case that it should be, but the unfortunate reality of the DOM is
     * currently that Element and HTMLElement get pretty mushed together. Limiting this
     * to HTMLElement probably requires some careful analysis to see what would break, 
     * and a heavy rewrite of both sQuery and Gadgets.
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
