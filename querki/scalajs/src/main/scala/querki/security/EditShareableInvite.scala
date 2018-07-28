package querki.security

import org.scalajs.dom.html

import scalatags.JsDom.all._
import rx._
import autowire._

import org.querki.gadgets._

import querki.api.ThingFunctions
import querki.data.ThingInfo
import querki.display.ButtonGadget
import querki.display.input._
import querki.editing.EditFunctions
import querki.editing.EditFunctions._
import querki.globals._
  
class OneInviteGadget(inviteIn: ThingInfo, role: ThingInfo)(implicit e: Ecology, ctx: Ctx.Owner) 
  extends OneItemGadget[ThingInfo](inviteIn)
{
  def displayName(invite: ThingInfo): String = invite.displayName
  def prepToEdit(invite: ThingInfo, completer: EditCompleter[ThingInfo]): Future[EditShareableInvite] =
    EditShareableInvite.prepToEdit(invite, role, completer)
}

class RoleInvitesList(invites: Seq[ThingInfo], role: ThingInfo)(implicit e: Ecology, ctx: Ctx.Owner)
  extends ItemListManager(
    invites, 
    "Shareable Invitations", 
    "Create a new Invitation",
    div(
      p("""A Shareable Invitation is a link that you can share with whoever you like -- by email, webpage, or however.
          |Anyone who clicks on that link will be able to join the Space, with this Role. Click on the name of an
          |Invitation to edit it, or to get the link.""".stripMargin)))
{
  def showItem(invite: ThingInfo) = new OneInviteGadget(invite, role)
  def prepToCreate(completer: EditCompleter[ThingInfo]) = EditShareableInvite.create(role, completer)
}

/**
 * This is the actual editor for Shareable Invitations.
 * 
 * TODO: this is sufficiently similar to the EditRolePanel that they likely can/should be refactored.
 */
class EditShareableInvite(
    inviteOpt: Option[ThingInfo],
    forRole: ThingInfo,
    completer: EditCompleter[ThingInfo]
  )(implicit val ecology: Ecology, ctx: Ctx.Owner) 
  extends Gadget[html.Div] with EcologyMember 
{
  lazy val Client = interface[querki.client.Client]
  lazy val DataAccess = interface[querki.data.DataAccess]
  
  lazy val std = DataAccess.std
  
  type InputGadgetRef = GadgetRef[InputGadget[_]]
  
  val nameInput = GadgetRef[InputGadget[_]]
  
  // TODO: lift the common concept of a "saveable" out of InputGadget, to actually be this
  // static value. Or create a "saveable" typeclass, as mentioned below:
  class RoleLinkSaver extends
    InputGadget(ecology) with NoAutoSave with ForProp
  {
    val prop = std.security.inviteRoleLink
    def values = List(forRole.oid2.underlying)
    def doRender = ???
    def hook = ???
  }
  // TODO: This is unreasonably complex. Should fields actually take a context bound instead, and
  // we can have typeclass instances for it?
  val roleSaver = GadgetRef[InputGadget[_]]
  roleSaver <= new RoleLinkSaver
  
  val fields: List[InputGadgetRef] = List(nameInput, roleSaver)
  
  val creating = inviteOpt.isEmpty
  def initialName = inviteOpt.map(_.displayName).getOrElse("")
  
  def changeMsgs(): List[PropertyChange] = {
    def oneSaveMsg(ref: InputGadgetRef): Option[PropertyChange] = ref.mapNow(_.propertyChangeMsg())
    fields.map(oneSaveMsg).flatten
  }
  def saveMsg(): PropertyChange = {
    MultiplePropertyChanges(changeMsgs())
  }
  
  def spacer = p(" ")
  
  def doRender() = 
    div(cls := "panel panel-default",
      div(cls := "panel-heading",
        div(
          if (creating)
            "Create new Shared Invitation"
          else
            span(b(initialName))
        )
      ),
      div(cls := "panel-body",
        form(
          // Edit the name of the Invite:
          div(cls := "form-group",
            label("Invitation Name"),
            nameInput <= 
              new TextInputGadget(Seq("form-control", "col-md-3"), value := initialName)
                with NoAutoSave
                with ForProp { val prop = std.basic.displayNameProp }
          ),
          
          div(
            new ButtonGadget(ButtonGadget.Primary, "Save")({() => 
              inviteOpt match {
                case Some(invite) => {
                  for {
                    result <- InputGadget.doSaveChange(invite.oid, saveMsg())
                    newInvite <- Client[ThingFunctions].getThingInfo(invite.oid).call()
                  }
                    completer.editComplete(Some(newInvite))
                }
                case None => {
                  for {
                    // TODO: This really ought to take a single PropertyChange, now that we have
                    // MultiplePropertyChanges:
                    newInvite <- Client[EditFunctions].create(std.security.sharedInviteModel, changeMsgs()).call()
                  }
                    completer.editComplete(Some(newInvite))
                }
              }
            }),
            
            " ",
            new ButtonGadget(ButtonGadget.Normal, "Cancel")({() =>
              completer.editComplete(None)
            })
          )
        )
      )
    )
}

object EditShareableInvite {
  def prepToEdit(invite: ThingInfo, forRole: ThingInfo, completer: EditCompleter[ThingInfo])(implicit ecology: Ecology, ctx: Ctx.Owner): Future[EditShareableInvite] = {
    val Client = ecology.api[querki.client.Client]
    
    for {
      dummy <- Future.successful(())
    }
      yield new EditShareableInvite(Some(invite), forRole, completer)
  }
  
  def create(forRole: ThingInfo, completer: EditCompleter[ThingInfo])(implicit ecology: Ecology, ctx: Ctx.Owner): Future[EditShareableInvite] = {
    for {
      dummy <- Future.successful(())
    }
      yield new EditShareableInvite(None, forRole, completer)
  }
}
