package querki.security

import scala.concurrent.Future

import org.scalajs.dom.html

import scalatags.JsDom.all._
import autowire._
import rx._

import org.querki.gadgets._

import querki.display.{ButtonGadget, TabGadget}
import querki.display.rx.RxText
import querki.editing.EditFunctions
import querki.globals._
import querki.pages.Page

class CustomRolesTab(
    customMap: RoleInfo,
    page: Page
  )(implicit val ecology: Ecology, ctx: Ctx.Owner)
  extends TabGadget(SharingPage.Tab.CustomRoles.entryName, "custom", "Roles") with EcologyMember
{
  lazy val Client = interface[querki.client.Client]
  lazy val Editing = interface[querki.editing.Editing]

  class CustomRoleManager(customRoles:RoleInfo) extends Gadget[html.Div] {
    val roleAdder = GadgetRef[RxText]
    
    def createRole(name:String) = {
      val change = Seq(EditFunctions.ChangePropertyValue(Editing.propPath(page.std.basic.displayNameProp), List(name)))
      
      for {
        role <- Client[EditFunctions].create(page.std.security.customRoleModel, change).call()
      }
        // For now, we're just going to reload, instead of trying to do anything clever:
        yield page.PageManager.reload()
    }
    
    def doRender() =
      div(
        h4("Existing Roles"),
        for {
          role <- customRoles.roles
          if (role.oid.underlying.length > 0)
        }
          yield p(role.displayName),
        h4("Create a new Custom Role"),
        roleAdder <= new RxText(cls:="form-control col-md-3"),
        " ", 
        new ButtonGadget(ButtonGadget.Warning, "Add Role", disabled := roleAdder.flatMapRxOrElse(_.length)(_ == 0, true)) ({ () =>
          createRole(roleAdder.get.text.now)
        })
      )
  }
  
  def tabContent =
    for {
      dummy <- Future.successful(())
      guts =
        div(
          h3("Custom Roles"),
          p(b("Advanced: "),
            """You can define special custom Roles for your Space, if you need more control. For the moment, you
              |can only use these Roles in the fine-grained permission system (that is, using them for permissions
              |such as Who Can Edit); in the future, we will allow you to define Space-wide permissions for people
              |with these Roles. Note that, for now, you can only add up to one of these Roles per Member.""".stripMargin),
          new CustomRoleManager(customMap)
        )
    }
      yield guts 
}
