package org.querki.facades.raty

import scala.scalajs.js
import org.scalajs.dom
import org.scalajs.jquery._
import org.querki.jsext._

trait RatyFacade extends js.Object {
  /**
   * Interpret this JQuery element (which should point to a div) as a Raty.
   */
  def raty(opts:RatyOptions):Any = ???
}

trait RatyOptions extends js.Object
/**
 * The Options that can be passed into the raty constructor. All comments are copied
 * directly from the Raty docs; assume an implicit [sic] throughout. For full
 * documentation, see:
 *   http://wbotelhos.com/raty
 * 
 * TODO: this is a partial Facade so far. Flesh it out, put it in an appropriate packed, and release it.
 */
class RatyOptionBuilder(val dict:OptMap) extends JSOptionBuilder[RatyOptions, RatyOptionBuilder](new RatyOptionBuilder(_)) {

  /**
   * Callback to handle the score and the click event on click action.
   */
  def click(v:js.ThisFunction2[dom.Element, Int, JQueryEventObject, Any]) = jsOpt("click", v)
  
  /**
   * Changes the hint for each star by it position on array.
   */
  def hints(v:js.Array[String]) = jsOpt("hints", v)
  
  /**
   * Changes the number of stars.
   */
  def number(v:Int) = jsOpt("number", v)
  
  /**
   * Changes the path where your icons are located.
   * Set it only if you want the same path for all icons.
   * Don't mind about the last slash of the path, if you don't put it, it will be setted for you.
   */
  def path(v:String) = jsOpt("path", v)
  def path(v:js.ThisFunction0[dom.Element, String]) = jsOpt("path", v)
  
  /**
   * You can prevent users to vote. It can be applied with or without score and all stars will
   *  receives the hint corresponding of the selected star.
   */
  def readOnly(v:Boolean) = jsOpt("readOnly", v)
  def readOnly(v:js.ThisFunction0[dom.Element, Boolean]) = jsOpt("readOnly", v)
  
  /**
   * Used when we want starts with a saved rating.
   */
  def score(v:Int) = jsOpt("score", v)
  def score(v:js.ThisFunction0[dom.Element, Int]) = jsOpt("score", v)
  
  /**
   * Some place to display the hints or the cancelHint.
   */
  def target(v:Selector) = jsOpt("target", v)
  
  /**
   * If you want to keep the score into the hint box after you do the rating, turn on this option.
   */
  def targetKeep(v:Boolean) = jsOpt("targetKeep", v)
}
object RatyOptions extends RatyOptionBuilder(noOpts)
