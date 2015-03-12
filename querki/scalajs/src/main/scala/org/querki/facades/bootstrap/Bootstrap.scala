package org.querki.facades.bootstrap

import scala.scalajs.js
import js.{Dynamic, UndefOr, undefined => undef}
import js.JSConverters._
import org.querki.jsext._
import org.querki.jquery._

/**
 * A very partial, quick-and-dirty facade for the parts of Bootstrap 2.3 that we are currently
 * using.
 * 
 * TODO: after Querki gets upgraded to Bootstrap 3, begin to evolve this into a proper open-source
 * facade, and encourage folks to help fill it in.
 */
trait BootstrapFacade extends js.Object {
  def modal(cmd:ModalCommand.ModalCommand):Any = js.native
  
  def popover(options:PopoverOptions):Any = js.native
  def popover(cmd:PopoverCommand.PopoverCommand):Any = js.native
  
  def tooltip(options:TooltipOptions):Any = js.native
  
  def collapse():JQuery = js.native
  def collapse(cmd:ModalCommand.ModalCommand):JQuery = js.native
}

// TODO: see the ManifestFacade for a stronger way to do this:
object ModalCommand {
  type ModalCommand = String
  
  val show = "show"
  val hide = "hide"
  val toggle = "toggle"
}

// TODO: does this actually work? When we pass this into the facade, does it receive a String?
// I suspect not.
object Position extends Enumeration {
  type Position = Value
  val left = Value("left")
  val right = Value("right")
  val top = Value("top")
  val bottom = Value("bottom")
}

object Trigger extends Enumeration {
  type Trigger = Value
  val click = Value("click")
  val hover = Value("hover")
  val focus = Value("focus")
  val manual = Value("manual")
}

// TODO: see the ManifestFacade for a stronger way to do this:
object PopoverCommand {
  type PopoverCommand = String
  
  val show = "show"
  val hide = "hide"
  val toggle = "toggle"
  val destroy = "destroy"
}

trait PopoverOptions extends js.Object 

class PopoverOptionBuilder(val dict:OptMap) extends JSOptionBuilder[PopoverOptions, PopoverOptionBuilder](new PopoverOptionBuilder(_)) {
  def animation(v:Boolean) = jsOpt("animation", v)
  def html(v:Boolean) = jsOpt("html", v)
  def placement(v:Position.Position) = jsOpt("placement", v.toString)
  def placement(v:js.Function0[Position.Position]) = jsOpt("placement", v)
  def selector(v:String) = jsOpt("selector", v)
  def trigger(v:Trigger.Trigger) = jsOpt("trigger", v.toString)
  def title(v:String) = jsOpt("title", v)
  def title(v:js.Function0[String]) = jsOpt("title", v)
  def content(v:String) = jsOpt("content", v)
  def delay(v:Int) = jsOpt("delay", v)
  def delay(v:PopoverDelay) = jsOpt("delay", v)
  def container(v:String) = jsOpt("container", v)
}
object PopoverOptions extends PopoverOptionBuilder(noOpts)

trait PopoverDelay extends js.Object
class PopoverDelayBuilder(val dict:OptMap) extends JSOptionBuilder[PopoverDelay, PopoverDelayBuilder](new PopoverDelayBuilder(_)) {
  def show(v:Int) = jsOpt("show", v)
  def hide(v:Int) = jsOpt("show", v)
}
object PopoverDelay extends PopoverDelayBuilder(noOpts)
