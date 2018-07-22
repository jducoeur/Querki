package querki.security

import scala.concurrent.Future

import org.scalajs.dom.html

import scalatags.JsDom.all._
import autowire._
import rx._

import org.querki.jquery._
import org.querki.gadgets._

import querki.data.ThingInfo
import querki.display.{ButtonGadget, TabGadget}
import querki.display.rx.RxText
import querki.editing.EditFunctions
import querki.globals._
import querki.pages.Page

trait RoleEditCompleter {
  def roleComplete(roleOpt: Option[ThingInfo]): Unit
}
  
class OneRoleGadget(roleIn: ThingInfo)(implicit val ecology: Ecology, ctx: Ctx.Owner) 
  extends Gadget[html.Div] with RoleEditCompleter 
{ roleGadget =>
  val role = Var(roleIn)
  
  class RoleDisplayGadget() extends Gadget[html.Anchor] {
    override def onCreate(e: html.Anchor) = {
      $(e).click { evt: JQueryEventObject =>
        EditRolePanel.prepToEdit(role.now, roleGadget).map { panel: Gadget[html.Div] =>
          roleDiv <= panel
        }
        evt.preventDefault()
      }      
    }
    
    def doRender() = a(href := "#", role.now.displayName)
  }
  
  val roleDiv = GadgetRef.of[html.Div]
  
  def displayRoleName() = 
    roleDiv <= div(
      new RoleDisplayGadget()
    )
  
  def roleComplete(newRoleOpt: Option[ThingInfo]) = {
    newRoleOpt.map(newRole => role() = newRole)
    displayRoleName()
  }
  
  def doRender() =
    div(
      displayRoleName()
    )
}

class CustomRolesTab(
    customMap: RoleInfo,
    page: Page
  )(implicit val ecology: Ecology, ctx: Ctx.Owner)
  extends TabGadget(SharingPage.Tab.CustomRoles.entryName, "custom", "Roles") with EcologyMember
{
  lazy val Client = interface[querki.client.Client]
  lazy val Editing = interface[querki.editing.Editing]

  class CustomRoleManager(customRoles:RoleInfo) extends Gadget[html.Div] with RoleEditCompleter {
    val addDiv = GadgetRef.of[html.Div]
    val roleDiv = GadgetRef.of[html.Div]
    
    def roleComplete(newRoleOpt: Option[ThingInfo]) = {
      roleDiv <= div()
      newRoleOpt.map { newRole =>
        addDiv.mapElemNow { e =>
          $(e).append((new OneRoleGadget(newRole)).render)
        }
      }
    }
    
    def doRender() =
      div(
        h4("Custom Roles"),
        for {
          role <- customRoles.roles
          if (role.oid.underlying.length > 0)
        }
          yield new OneRoleGadget(role),
          
        // New Roles will get placed in here:
        addDiv <= div(),
        // We stick the Create Role panel in here, when open:
        roleDiv <= div(),
        new ButtonGadget(ButtonGadget.Warning, "Add a new Custom Role") ({ () =>
          val panel = EditRolePanel.create(this)
          roleDiv <= panel
        })
      )
  }
  
  def tabContent =
    for {
      dummy <- Future.successful(())
      guts =
        div(
          h3("Custom Roles"),
          p("""You can create Custom Roles, and assign members to them, in order to give them specific Permissions. Permissions assigned to
              |the Role itself apply to all Things in this Space. You can also use the Security page for a Thing or Model to give a specific
              |Permission to a specific Role for that Thing or Model.""".stripMargin),
          p("""Click on a Role to edit it, or to create a Shared Link that will give that Role to anyone who clicks on it.""".stripMargin),
          new CustomRoleManager(customMap)
        )
    }
      yield guts 
}
