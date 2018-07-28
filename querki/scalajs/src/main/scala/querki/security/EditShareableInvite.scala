package querki.security

import org.scalajs.dom.html

import scalatags.JsDom.all._
import rx._
import autowire._

import org.querki.gadgets._

import querki.api.ThingFunctions
import querki.api.ThingFunctions._
import querki.data.{PropValInfo, ThingInfo, TOID}
import querki.display.ButtonGadget
import querki.display.input._
import querki.display.rx.RxCheckbox
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

import SaveablePropertyValue._

/**
 * This is the actual editor for Shareable Invitations.
 * 
 * TODO: this is sufficiently similar to the EditRolePanel that they likely can/should be refactored.
 */
class EditShareableInvite(
    inviteOpt: Option[ThingInfo],
    invitePropsOpt: Option[Map[TOID, PV]],
    forRole: ThingInfo,
    completer: EditCompleter[ThingInfo]
  )(implicit val ecology: Ecology, ctx: Ctx.Owner) 
  extends Gadget[html.Div] with EcologyMember 
{
  lazy val Client = interface[querki.client.Client]
  lazy val DataAccess = interface[querki.data.DataAccess]
  
  lazy val std = DataAccess.std
  
  // TBD: this approach to fetching the values from the server is still only so-so; in particular, the
  // way that the fetch and test are so separate from each and poorly typed. But it's an improvement...
  val requiresMembership: Var[Boolean] = 
    invitePropsOpt match {
      case Some(propMap) => {
        val current = for {
          inviteProps <- invitePropsOpt
          BoolV(vList) <- inviteProps.get(std.security.inviteRequiresMembership.oid2)
          v <- vList.headOption
        }
          yield v
        
        Var(current.getOrElse(false))
      }
      case None => Var(false)
    }
  
  type InputGadgetRef = GadgetRef[InputGadget[_]]
  
  val nameInput = GadgetRef[InputGadget[_]]
  
  val creating = inviteOpt.isEmpty
  def initialName = inviteOpt.map(_.displayName).getOrElse("")
  
  def changeMsgs(): List[PropertyChange] = {
    def s[T: SaveablePropertyValue](t: T) = t.getSaveable
    List(
      s(nameInput),
      s(HardcodedSaveable(std.security.inviteRoleLink, List(forRole.oid2.underlying))),
      s(SaveableRxBoolean(std.security.inviteRequiresMembership, requiresMembership))
    ).flatten
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
          
          div(cls := "form-group",
            label("Does this Invitation require login?"),
            p("""If you check this, people who use this Invitation will be required to sign up; they
                |cannot participate as anonymous guests.""".stripMargin),
            new RxCheckbox(requiresMembership, "Login required")
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
    val DataAccess = ecology.api[querki.data.DataAccess]
    val std = DataAccess.std
    
    for {
      inviteProps <- Client[ThingFunctions].getPropertyValues(invite, List(std.security.inviteRequiresMembership.oid2)).call()
    }
      yield new EditShareableInvite(Some(invite), Some(inviteProps), forRole, completer)
  }
  
  def create(forRole: ThingInfo, completer: EditCompleter[ThingInfo])(implicit ecology: Ecology, ctx: Ctx.Owner): Future[EditShareableInvite] = {
    for {
      dummy <- Future.successful(())
    }
      yield new EditShareableInvite(None, None, forRole, completer)
  }
}
