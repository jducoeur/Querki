package querki.security

import org.scalajs.dom.html

import scalatags.JsDom.all._
import rx._

import org.querki.gadgets._
import org.querki.jquery._

import querki.data.{TID, ThingInfo}
import querki.display.HookedGadget
import querki.display.rx.RxCheckbox
import querki.editing.EditFunctions.{AddToSet, RemoveFromSet}
import querki.globals._

case class OneRoleRx(
  role: ThingInfo,
  selected: Boolean = false
) {
  val id = role.oid
  val display = role.displayName
  val rx = Var(selected)
}

/**
 * This is the drop-down displayed when you click on the display of the current Custom Roles.
 */
class CustomRoleSelector(
  parent: CustomRoleDisplay,
  info: RoleInfo,
  val roles: Var[Seq[ThingInfo]]
)(implicit
  val ecology: Ecology,
  ctx: Ctx.Owner
) extends Gadget[html.Div] {
  val roleFlags = info.roles.map(role => OneRoleRx(role, roles.now.exists(_.oid.underlying == role.underlying)))
  val selectedRoleRxs = Rx { roleFlags.filter(_.rx()) }

  def path = parent.parent.path

  def recalcRoles() = {
    roles() = selectedRoleRxs.now.map(_.role)
  }

  // TODO: this doesn't combine properly with the way StandardRoleSelector works. This one sends diffs;
  // that one slams the entire value. Choose one way and commit to it.
  roleFlags.foreach { oneRole =>
    oneRole.rx.triggerLater {
      val selected = oneRole.rx.now
      val v = oneRole.id.underlying
      val msg =
        if (selected) {
          AddToSet(path, v)
        } else {
          RemoveFromSet(path, v)
        }
      parent.parent.saveChange(msg)
      recalcRoles()
    }
  }

  def doRender() =
    div(
      display := "none",
      cls := "panel panel-primary",
      div(cls := "panel-heading", div(cls := "panel-title", "Select Custom Roles")),
      div(
        cls := "panel-body",
        roleFlags.map { item =>
          div(
            new RxCheckbox(item.rx, item.display, cls := "_checkOption")
          )
        },
        button(cls := "btn btn-primary", "Done", onclick := { () => parent.roleChosen() })
      )
    )
}

class CustomRoleDisplay(
  val parent: RolesDisplay,
  initialRoles: Seq[TID],
  val tid: TID,
  roleInfo: RoleInfo
)(implicit
  e: Ecology,
  ctx: Ctx.Owner
) extends HookedGadget[html.Span](e) {
  def findInitial(info: RoleInfo): Seq[ThingInfo] = info.roles.filter(role => initialRoles.contains(role.oid))
  val roles = Var(findInitial(roleInfo))

  val roleName = Rx {
    val raw = roles().map(_.displayName).mkString(", ")
    if (raw.length == 0)
      "No Custom Roles Selected"
    else
      raw
  }
  val currentRoleIds = Rx { roles().map(_.oid.underlying) }
  val showingSelector = Var(false)

  def roleChosen() = {
    $(selector).hide(200)
    showingSelector() = false
    parent.save()
  }

  def hook() = {
    $(elem).click({ evt: JQueryEventObject =>
      if (showingSelector.now) {
        roleChosen()
      } else {
        $(selector).show(200)
        showingSelector() = true
      }
    })
  }

  def doRender() =
    span(cls := "_chooseCustomRole label label-info", data("personid") := tid.underlying, new RxTextFrag(roleName))

  lazy val roleSelector = new CustomRoleSelector(this, roleInfo, roles)

  lazy val selector = {
    // When we need the actual selector, we drop it in place of the placeholder:
    val e = (roleSelector).render
    parent.selectorArea <= roleSelector
    e
  }
}
