package querki.security

import scala.concurrent.Future

import org.scalajs.dom.html

import scalatags.JsDom.all._
import rx._
import autowire._

import org.querki.gadgets._

import querki.api.ThingFunctions
import querki.data.ThingInfo
import querki.editing.EditFunctions
import querki.editing.EditFunctions._
import querki.globals._
import querki.display.ButtonGadget
import querki.display.input._


/**
 * The panel for editing or creating a Role. Don't create this directly; use the helper functions in the
 * companion object. (Needed because we may need to fetch info from the server to create this.)
 * 
 * This panel is a bit experimental, in that it uses an explicit Save button. I'm not entirely sure that
 * I like this, now that I've done it -- it's inconsistent with the rest of Querki.
 */
private[security] class EditRolePanel(
    roleOpt: Option[ThingInfo],
    parent: RoleEditCompleter
  )(implicit val ecology: Ecology, ctx: Ctx.Owner) 
  extends Gadget[html.Div] with EcologyMember
{
  lazy val Client = interface[querki.client.Client]
  lazy val DataAccess = interface[querki.data.DataAccess]
  
  lazy val std = DataAccess.std
  
  type InputGadgetRef = GadgetRef[InputGadget[_]]
  val nameInput = GadgetRef[InputGadget[_]]
  
  val fields: List[InputGadgetRef] = List(nameInput)
  
  val creating = roleOpt.isEmpty
  def initialName = roleOpt.map(_.displayName).getOrElse("")
  
  def changeMsgs(): List[PropertyChange] = {
    def oneSaveMsg(ref: InputGadgetRef): Option[PropertyChange] = ref.mapNow(_.propertyChangeMsg())
    fields.map(oneSaveMsg).flatten
  }
  def saveMsg(): PropertyChange = {
    MultiplePropertyChanges(changeMsgs())
  }
  
  def doRender() = 
    div(cls := "panel panel-default",
      div(cls := "panel-heading",
        div(
          if (creating)
            "Create new Role"
          else
            span(b(initialName))
        )
      ),
      div(cls := "panel-body",
        form(
          div(cls := "form-group",
            label("Role Name"),
            nameInput <= 
              new TextInputGadget(Seq("form-control", "col-md-3"), value := initialName) 
                with NoAutoSave
                with ForProp { val prop = std.basic.displayNameProp }
          ),
          
          // TODO: list of checkboxes for the permissions of the role
          // TODO: list of the Shared Invites using this Role
          // TODO: modifiable list of the Members with this role
          
          div(
            new ButtonGadget(ButtonGadget.Primary, "Save")({() => 
              roleOpt match {
                case Some(role) => {
                  for {
                    result <- InputGadget.doSaveChange(role.oid, saveMsg())
                    newRole <- Client[ThingFunctions].getThingInfo(role.oid).call()
                  }
                    parent.roleComplete(newRole)
                }
                case None => {
                  for {
                    // TODO: This really ought to take a single PropertyChange, now that we have
                    // MultiplePropertyChanges:
                    newRole <- Client[EditFunctions].create(std.security.customRoleModel, changeMsgs()).call()
                  }
                    parent.roleComplete(newRole)
                }
              }
            })
          )
        )
      )
    )
}

object EditRolePanel {
  def prepToEdit(role: ThingInfo, parent: RoleEditCompleter)(implicit ecology: Ecology, ctx: Ctx.Owner): Future[EditRolePanel] = {
    for {
      // TODO: fetch the Role's permissions from the server:
      dummy <- Future.successful(())
    }
      yield new EditRolePanel(Some(role), parent)
  }
  
  def create(parent: RoleEditCompleter)(implicit ecology: Ecology, ctx: Ctx.Owner): EditRolePanel =
    new EditRolePanel(None, parent)
}
