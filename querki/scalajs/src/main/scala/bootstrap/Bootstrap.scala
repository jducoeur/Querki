package bootstrap

import scala.scalajs.js
import js.{Dynamic, UndefOr}
import js.JSConverters._

trait BootstrapFacade extends js.Object {
  def popover(options:PopoverOptions):Any = ???
  def popover(cmd:PopoverCommand.PopoverCommand):Any = ???
}

object PopoverCommand {
  type PopoverCommand = String
  
  val show = "show"
  val hide = "hide"
  val toggle = "toggle"
  val destroy = "destroy"
}

trait PopoverOptions extends js.Object {
  var animation:UndefOr[Boolean] = _
  var html:UndefOr[Boolean] = _
  // TODO: this is actually String|Function; how do we do this right?
  var placement:UndefOr[String] = _
  var selector:UndefOr[String] = _
  var trigger:UndefOr[String] = _
  var title:UndefOr[String] = _
  var content:UndefOr[String] = _
  var delay:UndefOr[Int] = _
  // TODO: this is String|false; how do we represent that?
  var container:UndefOr[Boolean] = _
}

object PopoverOptions {
  val undef:UndefOr[Nothing] = None.orUndefined
  
  def apply(
      content:UndefOr[String] = undef, 
      placement:UndefOr[String] = undef, 
      trigger:UndefOr[String] = undef) = 
  {
    Dynamic.literal(
      content = content,
      placement = placement,
      trigger = trigger
    ).asInstanceOf[PopoverOptions]
  }
}
