package querki.pages

import scala.scalajs.js
import js.JSConverters._
import scala.util.{Failure, Success}
import org.scalajs.dom.{raw => dom}
import org.querki.jquery._
import scalatags.JsDom.all._
import scalatags.JsDom.tags2.section
import autowire._
import rx._

import org.querki.facades.manifest._

import querki.globals._

import querki.api._
import querki.data.ThingInfo
import querki.display.{ButtonGadget, Gadget, RawDiv}
import querki.display.input.{InputGadget, LargeTextInputGadget, ManifestItem}
import querki.display.rx.{RunButton, RxTextFrag}
import querki.editing.EditFunctions
import querki.security.{PersonInfo, SecurityFunctions}

class SharingPage(implicit e:Ecology) extends Page(e) with EcologyMember {
  
  lazy val Client = interface[querki.client.Client]
  lazy val Editing = interface[querki.editing.Editing]
  lazy val StatusLine = interface[querki.display.StatusLine]
  
  lazy val space = DataAccess.space.get
  
  case class RoleInfo(map:Map[TID, ThingInfo], roles:Seq[ThingInfo]) {
    def default = roles.head
  }
  def makeRoleMap(roles:Seq[ThingInfo]) = RoleInfo(Map(roles.map(role => (role.oid -> role)):_*), roles)
  
    
  class RoleDisplay(initialRole:ThingInfo, tid:TID, roleInfo:RoleInfo, customInfo:RoleInfo) extends InputGadget[dom.HTMLSpanElement](ecology) {
    val role = Var(initialRole)
    val roleName = Rx(role().displayName)
      
    override lazy val thingId = tid
    override def path = Editing.propPath(std.security.personRolesProp.oid, Some(thingId))
    def values = List(role().oid.underlying)
      
    def roleChosen(roleId:String) = {
      role() = roleInfo.map(TID(roleId))
      $(selector).detachReplaceWith(elem)
      save()
    }
      
    def hook() = {
      $(elem).click({ evt:JQueryEventObject =>
        $(elem).detachReplaceWith(selector)
      })
    }
	        
    def doRender() =
      span(cls:="_chooseRole label label-info",
        data("personid"):=thingId.underlying,
        new RxTextFrag(roleName)
      )
        
    lazy val selector = (new RoleSelector).render
        
    class RoleSelector extends Gadget[dom.HTMLSelectElement] {
      def ecology = SharingPage.this.ecology
      
      override def onCreate(e:dom.HTMLSelectElement) = {
        $(elem).value(role().oid.underlying)
          
        $(elem).change({ evt:JQueryEventObject =>
          val chosen = $(elem).find(":selected").valueString
          roleChosen(chosen)
        })
      }
        
      def doRender() =
        select(
          for (r <- roleInfo.roles)
            yield option(value:=r.oid.underlying,
              r.displayName)
        )
    }
  }
  
  
  class PersonDisplay(showCls:String, person:PersonInfo, roleInfo:RoleInfo, customInfo:RoleInfo) extends Gadget[dom.HTMLTableRowElement] {
    def ecology = SharingPage.this.ecology
    
    val initPersonRole = person.roles.headOption.map(roleInfo.map(_)).getOrElse(roleInfo.default)
    
    def doRender() =
      tr(cls:=showCls,
	    td({
	      MSeq(
	        person.person.displayName, 
	        " -- ",
	        new RoleDisplay(initPersonRole, person.person.oid, roleInfo, customInfo)
	      )
	    })
	  )
  }
  
  class InviteeInput extends InputGadget[dom.HTMLInputElement](ecology) {
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
  
  class CollaboratorInput extends InputGadget[dom.HTMLInputElement](ecology) {
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
  
  def pageContent = for {
    securityInfo <- Client[SecurityFunctions].getSecurityInfo().call()
    (roles, custom) <- Client[SecurityFunctions].getRoles().call()
    inviteEditInfo <- Client[EditFunctions].getOnePropertyEditor(DataAccess.space.get.oid, std.security.inviteTextProp).call()
    roleMap = makeRoleMap(roles)
    customMap = makeRoleMap(custom)
    (members, invitees) <- Client[SecurityFunctions].getMembers().call()
    guts =
      div(
        section(id:="sendInvitations",
          h2("Send Invitations to Join this Space"),
          
          p("""Use this form to invite people to become Members of this Space. The Invitation Message may contain the usual Querki Text formatting,
          |including [[]]-style expressions; however, links may not yet work quite the way you expect.""".stripMargin),
          
          p("""Specify invitees by email address. Note that your current invitations are listed below. While it is acceptable to retry once or
          |twice, doing so excessively is frowned upon, as is sending unwelcome invitations. Either of these is considered spam, and is
          |grounds for removal from Querki.""".stripMargin),
          
          p(s"""Invitations will come from "${securityInfo.fromEmail}". If your invitations are getting spam-filtered, tell your invitees
          |to add that address to their Contacts.""".stripMargin),
          
          p("""Invitations will have your email address included as the Reply-To, so if the invitees write back, it will go directly to you.
          |(Eventually, we may have replies come to you via Querki, but for now, keep in mind that your invitees will see your email address.)""".stripMargin),
          
          new RawDiv(inviteEditInfo.editor),
          
          div(cls:="control-group",
            label(cls:="control-label", "Who to Invite by email (enter email addresses, comma-separated)"),
            div(cls:="controls",
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
              new RoleDisplay(roleMap.map(securityInfo.defaultRole), DataAccess.space.get.oid, roleMap, customMap)
            )
          ),
        
          div(cls:="control-group",
            div(cls:="controls",
              new RunButton(
                ButtonGadget.Normal, 
                "Invite Members",
                "Inviting...")
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
              })
            )
          )
        ),
        
        section(id:="invitees",
          h2("Outstanding Invitations"),
          p("The following invitations have been sent but not yet accepted."),
          
          table(cls:="table table-hover",
            tbody(
              for (member <- invitees) 
                yield new PersonDisplay("warning", member, roleMap, customMap)
            )
          )          
        ),
        
        section(id:="members",
          h2("Members"),
          p("The following people are members of this Space. Click on a member's Role in order to change it."),
          
          table(cls:="table table-hover",
            tbody(
              for (member <- members) 
                yield new PersonDisplay("info", member, roleMap, customMap)
            )
          )
        ),
        
        section(id:="custom",
          h2("Custom Roles"),
          p("""You can define special custom Roles for your Space, if you need more control. For the moment, you
              |can only use these Roles in the fine-grained permission system (that is, using them for permissions
              |such as Who Can Edit); in the future, we will allow you to define Space-wide permissions for people
              |with these Roles. Note that, for now, you can only add up to one of these Roles per Member.""".stripMargin))
      )
  }
    yield PageContents(s"Sharing for ${space.displayName}", guts)
}
