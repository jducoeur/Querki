package querki.security

import org.scalajs.dom.html

import scalatags.JsDom.all._
import rx._

import org.querki.facades.bootstrap._
import org.querki.gadgets._
import org.querki.jquery._

import querki.api.StandardThings
import querki.data.PropValInfo
import querki.display.input._
import querki.globals._
import querki.security.SecurityFunctions._

private[security] class PermCheckboxes(allPerms: Seq[PermInfo], thingPropsOpt: Option[Seq[PropValInfo]], std: StandardThings) 
  (implicit e: Ecology, ctx: Ctx.Owner)
  extends InputGadget[html.Div](e) with NoAutoSave with ForProp 
{
  val prop = std.security.rolePermissionsProp
  
  def hook() = {}
  
  def values: Seq[String] = 
    checkboxes
      .filter(oneChk => $(oneChk.chk.elem).prop("checked").asInstanceOf[Boolean])
      .map(_.perm.id.underlying)
  
  // TODO: I'd be much more comfortable with this if we had a good way to get OIDs instead of names here.
  // It works the way it does because that's what we get from ThingFunctions.getProperties(), but it
  // kind of sucks.
  val thingPermNames: List[String] = {
    (for {
      thingProps <- thingPropsOpt
      rolePermsProp <- thingProps.find(_.propInfo.oid == std.security.rolePermissionsProp.oid)
    }
      yield rolePermsProp.raw.lines.toList).getOrElse(List())
  }
  
  def hasPerm(perm: PermInfo) = thingPermNames.contains(perm.name)
  
  class OneCheckbox(val perm: PermInfo) extends Gadget[html.Div] {
    val name = GadgetRef.of[html.Anchor]
      .whenRendered { g =>
        $(g.elem).popover(PopoverOptions.trigger(Trigger.click))
      }
    val chk = GadgetRef.of[html.Input]
    
    def doRender() =
      div(cls := "row",
        span(cls := "col-md-1",
          chk <= input(
            tpe := "checkbox",
            if (hasPerm(perm))
              checked := "checked"
          )
        ),
        name <= a(cls := "", data("toggle") := "popover", data("content") := perm.summary, perm.name)
      )
  }
  
  val checkboxes = 
    for (perm <- allPerms)
      yield new OneCheckbox(perm)
  
  def doRender() =
    div(cls := "container",
      p(b("This Role gives its members these permissions throughout this Space")),
      checkboxes
    )
}
