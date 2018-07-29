package querki.security

import org.scalajs.dom
import org.scalajs.dom.html

import scalatags.JsDom.all._
import rx._
import autowire._

import org.querki.gadgets._
import org.querki.jquery._

import querki.api.ThingFunctions
import querki.api.ThingFunctions._
import querki.data.{PropValInfo, ThingInfo, TOID}
import querki.display.{ButtonGadget, Dialog, QuerkiUIUtils}
import querki.display.input._
import querki.display.rx.RxCheckbox
import querki.editing.EditFunctions
import querki.editing.EditFunctions._
import querki.globals._

import SecurityFunctions._

class ShareableInviteUrlButton(invite: SharedLinkInfo)(implicit val ecology: Ecology)
  extends Gadget[html.Element] with QuerkiUIUtils with EcologyMember
{
  lazy val Client = interface[querki.client.Client]
  
  val urlDisplay = GadgetRef.of[html.Input]
  def copyUrlToClipboard() = {
    urlDisplay.mapElemNow { e =>
      $(e).focus()
      $(e).select()
      // Yes, really, this seems to be the standard way to make this work. *Sigh*.
      dom.window.document.execCommand("copy")
    }
  }
  
  def showDialogForUrl(url: String) = {
    val d = 
      new Dialog(
        s"Share Invitation ${invite.thingInfo.displayName}",
        div(
          p("Press the Copy button to copy the Link to your clipboard"),
          div(cls := "input-group",
            urlDisplay <= input(
              tpe := "text",
              cls := "form-control",
              value := url),
            span(cls := "input-group-btn",
              button(cls := "btn btn-default",
                tpe := "button",
                "Copy",
                onclick := { () => copyUrlToClipboard() } 
              )
            )
          )
        ),
        (ButtonGadget.Normal, Seq("Done"), { dialog => dialog.done() })
      )
    d.show()
  }
  
  override def onCreate(e: html.Element) = {
    $(e).click { evt: JQueryEventObject =>
      for {
        url <- Client[SecurityFunctions].getSharedLinkURL(invite.thingInfo.oid2).call()
      }
        showDialogForUrl(url)
    }
  }
  
  def doRender() = faIconButton("share-alt", Seq("btn-xs"))
}
  
class OneInviteGadget(inviteIn: SharedLinkInfo, role: ThingInfo)(implicit e: Ecology, ctx: Ctx.Owner) 
  extends OneItemGadget[SharedLinkInfo](inviteIn)
{
  def displayName(invite: SharedLinkInfo): String = invite.thingInfo.displayName
  override def listingButtons(current: SharedLinkInfo) = 
    Some(span(" ", new ShareableInviteUrlButton(current)))
  def prepToEdit(invite: SharedLinkInfo, completer: EditCompleter[SharedLinkInfo]): Future[EditShareableInvite] =
    EditShareableInvite.prepToEdit(invite, role, completer)
}

class RoleInvitesList(invites: Seq[SharedLinkInfo], role: ThingInfo)(implicit e: Ecology, ctx: Ctx.Owner)
  extends ItemListManager(
    invites, 
    "Shareable Invitations", 
    "Create a new Invitation",
    div(
      p("""A Shareable Invitation is a link that you can share with whoever you like -- by email, webpage, or however.
          |Anyone who clicks on that link will be able to join the Space, with this Role. Click on the name of an
          |Invitation to edit it, or its """.stripMargin,
        a(cls := "btn btn-default btn-xs querki-icon-button", i(cls := "fa fa-share-alt fa-lg")),
        " button to get the shareable link.")))
{
  def showItem(invite: SharedLinkInfo) = new OneInviteGadget(invite, role)
  def prepToCreate(completer: EditCompleter[SharedLinkInfo]) = EditShareableInvite.create(role, completer)
}

import SaveablePropertyValue._

/**
 * This is the actual editor for Shareable Invitations.
 * 
 * TODO: this is sufficiently similar to the EditRolePanel that they likely can/should be refactored.
 */
class EditShareableInvite(
    inviteOpt: Option[SharedLinkInfo],
    forRole: ThingInfo,
    completer: EditCompleter[SharedLinkInfo]
  )(implicit val ecology: Ecology, ctx: Ctx.Owner) 
  extends Gadget[html.Div] with EcologyMember 
{
  lazy val Client = interface[querki.client.Client]
  lazy val DataAccess = interface[querki.data.DataAccess]
  
  lazy val std = DataAccess.std
  
  // TBD: this approach to fetching the values from the server is still only so-so; in particular, the
  // way that the fetch and test are so separate from each and poorly typed. But it's an improvement...
  val requiresMembership: Var[Boolean] = 
    inviteOpt match {
      case Some(invite) => Var(invite.requiresMembership)
      case None => Var(false)
    }
  
  type InputGadgetRef = GadgetRef[InputGadget[_]]
  
  val nameInput = GadgetRef[InputGadget[_]]
  
  val creating = inviteOpt.isEmpty
  def initialName = inviteOpt.map(_.thingInfo.displayName).getOrElse("")
  
  def changeMsgs(): List[PropertyChange] = {
    def s[T: SaveablePropertyValue](t: T) = t.getSaveable
    List(
      s(nameInput),
      s(HardcodedSaveable(std.security.inviteRoleLink, List(forRole.oid2.underlying))),
      s(SaveableRxBoolean(std.security.inviteRequiresMembership, requiresMembership)),
      if (creating)
        s(HardcodedSaveable(std.security.isOpenInvite, List("on")))
      else
        None
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
                    result <- InputGadget.doSaveChange(invite.thingInfo.oid, saveMsg())
                    newInvite <- Client[SecurityFunctions].getOneSharedLink(invite.thingInfo.oid2).call()
                  }
                    completer.editComplete(Some(newInvite))
                }
                case None => {
                  for {
                    // TODO: This really ought to take a single PropertyChange, now that we have
                    // MultiplePropertyChanges:
                    newInviteThing <- Client[EditFunctions].create(std.security.sharedInviteModel, changeMsgs()).call()
                    newInvite <- Client[SecurityFunctions].getOneSharedLink(newInviteThing.oid2).call()
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
  def prepToEdit(invite: SharedLinkInfo, forRole: ThingInfo, completer: EditCompleter[SharedLinkInfo])(implicit ecology: Ecology, ctx: Ctx.Owner): Future[EditShareableInvite] = {
    for {
      dummy <- Future.successful(())
    }
      yield new EditShareableInvite(Some(invite), forRole, completer)
  }
  
  def create(forRole: ThingInfo, completer: EditCompleter[SharedLinkInfo])(implicit ecology: Ecology, ctx: Ctx.Owner): Future[EditShareableInvite] = {
    for {
      dummy <- Future.successful(())
    }
      yield new EditShareableInvite(None, forRole, completer)
  }
}
