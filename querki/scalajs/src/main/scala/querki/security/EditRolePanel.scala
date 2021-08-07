package querki.security

import scala.concurrent.Future

import org.scalajs.dom.html

import scalatags.JsDom.all._
import rx._
import autowire._

import org.querki.gadgets._
import org.querki.jquery._

import querki.api.ThingFunctions
import querki.data.{PropValInfo, ThingInfo}
import querki.display.ButtonGadget
import querki.display.input._
import querki.editing.EditFunctions
import querki.editing.EditFunctions._
import querki.globals._
import querki.security.SecurityFunctions._
import querki.util.ScalatagUtils

import SaveablePropertyValue._

/**
 * The panel for editing or creating a Role. Don't create this directly; use the helper functions in the
 * companion object. (Needed because we may need to fetch info from the server to create this.)
 */
private[security] class EditRolePanel(
  allPerms: Seq[PermInfo],
  roleOpt: Option[ThingInfo],
  rolePropsOpt: Option[Seq[PropValInfo]],
  invites: Seq[SharedLinkInfo],
  parent: EditCompleter[ThingInfo]
)(implicit
  val ecology: Ecology,
  ctx: Ctx.Owner
) extends Gadget[html.Div]
     with EcologyMember
     with ScalatagUtils {
  lazy val Client = interface[querki.client.Client]
  lazy val DataAccess = interface[querki.data.DataAccess]

  lazy val std = DataAccess.std

  type InputGadgetRef = GadgetRef[InputGadget[_]]

  val nameInput = GadgetRef[InputGadget[_]]
  val permCheckboxes = GadgetRef[InputGadget[_]]

  val creating = roleOpt.isEmpty
  def initialName = roleOpt.map(_.displayName).getOrElse("")

  def changeMsgs(): List[PropertyChange] = {
    def s[T : SaveablePropertyValue](t: T) = t.getSaveable
    List(
      s(nameInput),
      s(permCheckboxes)
    ).flatten
  }

  def saveMsg(): PropertyChange = {
    MultiplePropertyChanges(changeMsgs())
  }

  def spacer = p(" ")

  def doRender() =
    div(
      cls := "panel panel-default",
      div(
        cls := "panel-heading",
        div(
          if (creating)
            "Create new Role"
          else
            span(b(initialName))
        )
      ),
      div(
        cls := "panel-body",
        form(
          // Edit the name of the Role:
          div(
            cls := "form-group",
            label("Role Name"),
            nameInput <=
              new TextInputGadget(Seq("form-control", "col-md-3"), value := initialName) with NoAutoSave with ForProp {
                val prop = std.basic.displayNameProp
              }
          ),
          // The permissions for this Role:
          permCheckboxes <= new PermCheckboxes(allPerms, rolePropsOpt, std),
          spacer,
          // The Shared Invitations for this Role (doesn't actually participate in Save. You have
          // to create the Role first, and then add Invitations for it.
          // TODO: this is awkward UX. How can we make it better? Ideally, I'd like to be able to
          // set up the invitation as part of creating the Role.
          roleOpt.map { role =>
            MSeq(
              new RoleInvitesList(invites, role),
              spacer
            )
          },
          // TODO: modifiable list of the Members with this role
          div(
            new ButtonGadget(ButtonGadget.Primary, "Save")({ () =>
              roleOpt match {
                case Some(role) => {
                  for {
                    result <- InputGadget.doSaveChange(role.oid, saveMsg())
                    newRole <- Client[ThingFunctions].getThingInfo(role.oid).call()
                  } parent.editComplete(Some(newRole))
                }
                case None => {
                  for {
                    // TODO: This really ought to take a single PropertyChange, now that we have
                    // MultiplePropertyChanges:
                    newRole <- Client[EditFunctions].create(std.security.customRoleModel, changeMsgs()).call()
                  } parent.editComplete(Some(newRole))
                }
              }
            }),
            " ",
            new ButtonGadget(ButtonGadget.Normal, "Cancel")({ () =>
              parent.editComplete(None)
            })
          )
        )
      )
    )
}

object EditRolePanel {

  def prepToEdit(
    role: ThingInfo,
    parent: EditCompleter[ThingInfo]
  )(implicit
    ecology: Ecology,
    ctx: Ctx.Owner
  ): Future[EditRolePanel] = {
    val Client = ecology.api[querki.client.Client]

    for {
      allPerms <- Client[SecurityFunctions].getAllPerms().call()
      thingProps <- Client[ThingFunctions].getProperties(role).call()
      invites <- Client[SecurityFunctions].getSharedLinksForRole(role.oid2).call()
    } yield new EditRolePanel(allPerms, Some(role), Some(thingProps), invites, parent)
  }

  def create(
    parent: EditCompleter[ThingInfo]
  )(implicit
    ecology: Ecology,
    ctx: Ctx.Owner
  ): Future[EditRolePanel] = {
    val Client = ecology.api[querki.client.Client]

    for {
      allPerms <- Client[SecurityFunctions].getAllPerms().call()
    } yield new EditRolePanel(allPerms, None, None, Seq.empty, parent)
  }
}
