package querki.security

import scala.scalajs.js
import js.JSConverters._
import scala.util.{Success, Failure}

import org.scalajs.dom.html

import scalatags.JsDom.all._
import autowire._
import rx._

import org.querki.facades.manifest._
import org.querki.gadgets._
import org.querki.jquery._

import querki.api._
import querki.display.{ButtonGadget, RawDiv, TabGadget}
import querki.display.input.{InputGadget, ManifestItem}
import querki.display.rx.RunButton
import querki.editing.EditFunctions
import querki.editing.EditFunctions.PropEditInfo
import querki.globals._
import querki.identity.UserLevel._
import querki.pages.{Page, PageImplicits}

class InvitationTab(
    roleMap: RoleInfo,
    customMap: RoleInfo,
    invitees: Seq[PersonInfo],
    page: Page
  )(implicit val ecology: Ecology, ctx: Ctx.Owner) 
  extends TabGadget(SharingPage.Tab.Invite.entryName, "sendInvitations", "Invites") with EcologyMember with PageImplicits
{
  lazy val Client = interface[querki.client.Client]
  lazy val DataAccess = interface[querki.data.DataAccess]
  lazy val PageManager = interface[querki.display.PageManager]
  lazy val StatusLine = interface[querki.display.StatusLine]
  lazy val UserAccess = interface[querki.identity.UserAccess]
  
  def std = page.std
  
  class InviteeInput extends InputGadget[html.Input](ecology) {
    def doRender() = input(tpe:="text", id:="invitees", name:="inviteesRaw")
    
    // TODO: validate that the email addresses input here are properly formatted
      
    def hook() = {
      // Invitees use the Manifest UI, but don't actually do any MarcoPolo'ing:
      $(elem).manifest(ManifestOptions.
        marcoPolo(false).
        // 188 is the *keycode* for comma:
        separator(Seq[Int](13, ',', 188).toJSArray).
        valuesName("invitees")
      )
    }
    def values = $(elem).manifestValues().toList
  }
  lazy val inviteeInput = new InviteeInput
  
  class CollaboratorInput extends InputGadget[html.Input](ecology) {
    def doRender() = input(tpe:="text", id:="collaborators", name:="collaboratorsRaw")
    
    def hook() = {
      // TODO: this still calls the old _getCollaborators entry point, which is potentially grungy. This should
      // get rewritten to call a more-official Client API instead.
      $(elem).manifest(ManifestOptions.
        marcoPolo(
          MarcoPoloOptions.
            url("_getCollaborators").
            minChars(3).
            required(true).
            formatData({ (data:js.Array[js.Object]) => data }).
            formatItem({ data:js.Dynamic => data.display }).
            formatNoResults({ q:String => s"No collaborator named $q found.".asInstanceOf[js.Any] })
        ).
        // 188 is the *keycode* for comma:
        separator(Seq[Int](13, ',', 188).toJSArray).
        required(true).
        valuesName("collaborators").
        formatDisplay({ data:js.Any => data.asInstanceOf[ManifestItem].display }:Function1[js.Any, js.Any]).
        formatValue({ data:js.Object => data.asInstanceOf[ManifestItem].id })
      )      
    }
    
    def values = $(elem).manifestValues().toList
  }
  lazy val collaboratorInput = new CollaboratorInput
  
  val customDisplay = GadgetRef.of[html.Div]

  def tabContent =
    for {
      securityInfo <- Client[SecurityFunctions].getSecurityInfo().call()
      inviteEditInfo <- Client[EditFunctions].getOnePropertyEditor(DataAccess.space.get.oid, std.security.inviteTextProp).call()
      awaitingValidation = (DataAccess.request.userLevel == PendingUser)
      guts = 
        div(
          h3("Send Invitations to Join this Space"),
          
          p("""Use this form to invite people to become Members of this Space. The Invitation Message may contain the usual Querki Text formatting,
          |including [[]]-style expressions; however, links may not yet work quite the way you expect.""".stripMargin),
          
          p("""Specify invitees by email address. Note that your current invitations are listed below. While it is acceptable to retry once or
          |twice, doing so excessively is frowned upon, as is sending unwelcome invitations.""".stripMargin, br(), 
          em("Either of these is considered spam, and is grounds for removal from Querki.")),
          
          p(s"""Invitations will come from "${securityInfo.fromEmail}". If your invitations are getting spam-filtered, tell your invitees
          |to add that address to their Contacts.""".stripMargin),
          
          p("""Invitations will have your email address included as the Reply-To, so if the invitees write back, it will go directly to you.
          |(Eventually, we may have replies come to you via Querki, but for now, keep in mind that your invitees will see your email address.)""".stripMargin),
          
          p(b("""Invitation Text to include in the email:""")),
          
          new RawDiv(inviteEditInfo.editor),
          
          div(cls:="control-group",
            label(cls:="control-label", "Who to Invite by email (enter email addresses, comma-separated):"),
            div(id := "_inviteeControls", cls:="controls",
              inviteeInput
            )
          ),
          
          div(cls:="control-group",
            label(cls:="control-label", "Or give the names or handles of collaborators you know from other Spaces:"),
            div(cls:="controls",
              collaboratorInput
            )
          ),
        
          div(cls:="control-group",
            div(cls:="controls",
              "These people should be invited as ",
              new RolesDisplay(securityInfo.defaultRoles, DataAccess.space.get.oid, roleMap, customMap, customDisplay, std),
              customDisplay <= div(display := "None")
            )
          ),
        
          div(cls:="control-group",
            div(cls:="controls",
              new RunButton(
                ButtonGadget.Normal, 
                "Invite Members",
                "Inviting...",
                id := "_inviteButton", 
                if (awaitingValidation) {disabled:=true})
              ({ btn =>
                val emails = inviteeInput.values
                val collabs = collaboratorInput.values.map(TID(_))
                Client[SecurityFunctions].invite(emails, collabs).call().onComplete {
                  case Success(response) => {
                    // TODO: we really want to make a prettier display for this message, probably as part
                    // of a general rewrite of StatusLine:
                    val allInvites = response.newInvites ++ response.resends
                    PageManager.reload().flashing(false, s"Sent invites to ${allInvites.mkString(", ")}")
                  }
                  case Failure(ex) => {
                    btn.done()
                    ex match {
                      case MaxMembersPerSpaceException(maxMembers) => {
                        StatusLine.showUntilChange(s"Sorry: at the moment you are limited to $maxMembers members per Space, and this would make more than that.")
                      }
                      case _ => {
                        StatusLine.showUntilChange(s"Error while trying to invite members!")
                      }
                    }
                  }
                }
              }),
              if (awaitingValidation) {
                page.flash(true, page.msg("notAllowedYet"), " ", UserAccess.resendActivationButton)
              }
            )
          ),
           
          h3("Outstanding Invitations"),
          p("The following invitations have been sent but not yet accepted."),
          
          table(cls:="table table-hover",
            tbody(
              for (member <- invitees.sortBy(_.person.displayName)) 
                yield new PersonDisplay("warning", member, roleMap, customMap, std, page)
            )
          )
        )
    }
      yield guts
}
