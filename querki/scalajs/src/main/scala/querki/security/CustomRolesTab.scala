package querki.security

import scala.concurrent.Future

import scalatags.JsDom.all._
import rx._

import querki.data.ThingInfo
import querki.display.TabGadget
import querki.globals._
import querki.pages.Page
 
class OneRoleGadget(roleIn: ThingInfo)(implicit e: Ecology, ctx: Ctx.Owner) 
  extends OneItemGadget[ThingInfo](roleIn)
{
  def displayName(current: ThingInfo): String = current.displayName
  def prepToEdit(current: ThingInfo, completer: EditCompleter[ThingInfo]): Future[EditRolePanel] =
    EditRolePanel.prepToEdit(current, completer)
}

class CustomRoleList(customRoles:RoleInfo)(implicit e: Ecology, ctx: Ctx.Owner)
  extends ItemListManager(
    customRoles.roles, 
    "Custom Roles", 
    "Add a new Custom Role",
    div(
      p("""You can create Custom Roles, and assign members to them, in order to give them specific Permissions. Permissions assigned to
          |the Role itself apply to all Things in this Space. You can also use the Security page for a Thing or Model to give a specific
          |Permission to a specific Role for that Thing or Model.""".stripMargin),
      p("""Click on a Role to edit it, or to create a Shared Link that will give that Role to anyone who clicks on it.""".stripMargin)))
{
  def showItem(role: ThingInfo) = new OneRoleGadget(role)
  def prepToCreate(completer: EditCompleter[ThingInfo]) = EditRolePanel.create(completer)
}

class CustomRolesTab(
    customMap: RoleInfo,
    page: Page
  )(implicit val ecology: Ecology, ctx: Ctx.Owner)
  extends TabGadget(SharingPage.Tab.CustomRoles.entryName, "custom", "Roles") with EcologyMember
{
  def tabContent = Future.successful(div(new CustomRoleList(customMap)))
}
