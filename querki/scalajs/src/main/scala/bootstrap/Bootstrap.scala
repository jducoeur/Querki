package bootstrap

import scala.scalajs.js
import js.{Dynamic, UndefOr, undefined => undef}
import js.JSConverters._
import org.scalajs.ext._

trait BootstrapFacade extends js.Object {
  def popover(options:PopoverOptions):Any = ???
  def popover(cmd:PopoverCommand.PopoverCommand):Any = ???
  
  def tooltip(options:TooltipOptions):Any = ???
}

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
