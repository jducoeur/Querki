package org.querki.facades.bootstrap.filestyle

import scala.scalajs.js

import org.querki.jsext._
import org.querki.jquery._

class BootstrapFilestyle extends js.Object {
  def filestyle(options:BootstrapFilestyleOptions):JQuery = js.native
}

trait BootstrapFilestyleOptions extends js.Object
object BootstrapFilestyleOptions extends BootstrapFilestyleOptionBuilder(noOpts)

/**
 * Options available to BootstrapFilestyle.
 * 
 * See http://markusslima.github.io/bootstrap-filestyle/ for full details.
 */
class BootstrapFilestyleOptionBuilder(val dict:OptMap) extends JSOptionBuilder[BootstrapFilestyleOptions, BootstrapFilestyleOptionBuilder](new BootstrapFilestyleOptionBuilder(_)) {
  def badge(v:Boolean) = jsOpt("badge", v)
  
  def buttonBefore(v:Boolean) = jsOpt("buttonBefore", v)
  
  def buttonName(v:String) = jsOpt("buttonName", v)
  
  def buttonText(v:String) = jsOpt("buttonText", v)
  
  def disabled(v:Boolean) = jsOpt("disabled", v)
  
  def icon(v:Boolean) = jsOpt("icon", v)
  
  def iconName(v:String) = jsOpt("iconName", v)
  
  def input(v:Boolean) = jsOpt("input", v)
  
  def size(v:ButtonSize) = jsOpt("size", v.underlying)
}

case class ButtonSize(underlying:String) extends AnyVal
object ButtonSize {
  val Large = ButtonSize("lg")
  val Normal = ButtonSize("nr")
  val Small = ButtonSize("sm")
}
