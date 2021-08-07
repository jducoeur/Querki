package querki.security

import org.scalajs.dom
import org.scalajs.dom.html

import scalatags.JsDom.all._
import rx._

import org.querki.gadgets._
import org.querki.jquery._

import querki.data.{TID, ThingInfo}
import querki.display.HookedGadget
import querki.globals._

class StandardRoleSelector(
  parent: StandardRoleDisplay,
  info: RoleInfo,
  val role: Var[ThingInfo]
)(implicit
  ctx: Ctx.Owner
) extends Gadget[html.Select] {
  val roleName = Rx { role().displayName }

  override def onCreate(e: html.Select) = {
    $(elem).value(role.now.oid.underlying)

    $(elem).change({ evt: JQueryEventObject =>
      val chosen = $(elem).find(":selected").valueString
      role() = info.map(TID(chosen))
      parent.roleChosen()
    })
  }

  def doRender() =
    select(
      for (r <- info.roles)
        yield option(value := r.oid.underlying, r.displayName)
    )
}

class StandardRoleDisplay(
  parent: RolesDisplay,
  initialRoles: Seq[TID],
  tid: TID,
  roleInfo: RoleInfo
)(implicit
  e: Ecology,
  ctx: Ctx.Owner
) extends HookedGadget[html.Span](e) {

  def findInitial(info: RoleInfo): ThingInfo =
    info.roles.find(role => initialRoles.contains(role.oid)).getOrElse(info.default)

  def roleChosen() = {
    $(selector).detachReplaceWith(elem)
    parent.save()
  }

  def hook() = {
    $(elem).click({ evt: JQueryEventObject =>
      $(elem).detachReplaceWith(selector)
    })
  }

  def doRender() =
    span(
      cls := "_chooseRole label label-info",
      data("personid") := tid.underlying,
      new RxTextFrag(roleSelector.roleName)
    )

  val roleSelector = new StandardRoleSelector(this, roleInfo, Var(findInitial(roleInfo)))
  lazy val selector = (roleSelector).render

  def curValue: Option[String] = {
    val raw = roleSelector.role.now.oid.underlying
    if (raw.length == 0)
      None
    else
      Some(raw)
  }
}
