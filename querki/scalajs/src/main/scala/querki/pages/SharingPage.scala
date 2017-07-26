package querki.pages

import scala.scalajs.js
import js.JSConverters._
import scala.util.{Failure, Success}
import org.scalajs.dom.{raw => dom}
import org.querki.gadgets._
import org.querki.jquery._
import scalatags.JsDom.all._
import scalatags.JsDom.tags2.section
import autowire._
import rx._
import enumeratum._

import org.querki.facades.manifest._

import querki.globals._

import querki.api._
import querki.data.ThingInfo
import querki.display.{ButtonGadget, HookedGadget, RawDiv}
import querki.display.input.{InputGadget, LargeTextInputGadget, ManifestItem}
import querki.display.rx._
import querki.editing.EditFunctions
import querki.identity.UserLevel._
import querki.security.{PersonInfo, SecurityFunctions}
import querki.session.UserFunctions

class SharingPage(params:ParamMap)(implicit val ecology:Ecology) extends Page("sharing") {
  
  import SharingPage._
  
  lazy val Client = interface[querki.client.Client]
  lazy val Editing = interface[querki.editing.Editing]
  lazy val SkillLevel = interface[querki.identity.skilllevel.SkillLevel]
  lazy val StatusLine = interface[querki.display.StatusLine]
  lazy val UserAccess = interface[querki.identity.UserAccess]
  
  lazy val space = DataAccess.space.get
  
  lazy val isAdvanced = SkillLevel.current == SkillLevel.AdvancedComplexity
  
  // TODO: the tab code here is pretty ugly and duplicative. We should really pull it all out
  // into a standard mechanism.
  lazy val initTabNameOpt = params.get("tab")
  lazy val initTab = initTabNameOpt.flatMap(Tab.withNameOption(_)).getOrElse(Tab.Invite)
  
  def tabCls(tab:Tab) = {
    if (initTab == tab)
      cls:="tab-pane active"
    else
      cls:="tab-pane"
  }
  
  val noRole = ThingInfo(
    TID(""),
    Some("None"),
    models.QWikitext("No Custom Role"),
    TID(""),
    models.Kind.Thing,
    false, false, false, false, false,
    None, Set.empty, Set.empty
  )
  
  case class RoleInfo(map:Map[TID, ThingInfo], roles:Seq[ThingInfo]) {
    def default = roles.head
    def isEmpty = roles.find(_.oid.underlying.length > 0).isEmpty
  }
  def makeRoleMap(roles:Seq[ThingInfo]) = RoleInfo(Map(roles.map(role => (role.oid -> role)):_*), roles)
      
  // TODO: this should probably become an RxSelect instead?
  class RoleSelector(parent:RoleDisplay, info:RoleInfo, val role:Var[ThingInfo]) extends Gadget[dom.HTMLSelectElement] {
    val roleName = Rx(role().displayName)
    
    override def onCreate(e:dom.HTMLSelectElement) = {
      $(elem).value(role().oid.underlying)
        
      $(elem).change({ evt:JQueryEventObject =>
        val chosen = $(elem).find(":selected").valueString
        role() = info.map(TID(chosen))
        parent.roleChosen()
      })
    }
      
    def doRender() =
      select(
        for (r <- info.roles)
          yield option(value:=r.oid.underlying,
            r.displayName)
      )
  }
  
  class RoleDisplay(parent:RolesDisplay, initialRoles:Seq[TID], tid:TID, roleInfo:RoleInfo) extends HookedGadget[dom.HTMLSpanElement](ecology) {
    def findInitial(info:RoleInfo):ThingInfo = info.roles.find(role => initialRoles.contains(role.oid)).getOrElse(info.default)
      
    def roleChosen() = {
      $(selector).detachReplaceWith(elem)
      parent.save()
    }
      
    def hook() = {
      $(elem).click({ evt:JQueryEventObject =>
        $(elem).detachReplaceWith(selector)
      })
    }
          
    def doRender() =
      span(cls:="_chooseRole label label-info",
        data("personid"):=tid.underlying,
        new RxTextFrag(roleSelector.roleName)
      )
        
    val roleSelector = new RoleSelector(this, roleInfo, Var(findInitial(roleInfo)))
    lazy val selector = (roleSelector).render
    
    def curValue:Option[String] = {
      val raw = roleSelector.role().oid.underlying
      if (raw.length == 0)
        None
      else
        Some(raw)
    }
  }
  
  class RolesDisplay(initialRoles:Seq[TID], tid:TID, roleInfo:RoleInfo, customInfo:RoleInfo) extends InputGadget[dom.HTMLSpanElement](ecology) {
    override lazy val thingId = tid
    override def path = Editing.propPath(std.security.personRolesProp.oid, Some(thingId))
    def values = List(roleDisplay.curValue, customDisplayRef.opt().flatMap(_.curValue)).flatten
    
    val roleDisplay = new RoleDisplay(this, initialRoles, tid, roleInfo)
    val customDisplayRef = QGadgetRef[RoleDisplay](ecology)
    
    def doRender() =
      span(
        roleDisplay,
        if (!customInfo.isEmpty) {
          MSeq(
            " and ",
            customDisplayRef <= new RoleDisplay(this, initialRoles, tid, customInfo))
        }
      )
      
    def hook() = {}
  }
  
  class PersonDisplay(showCls:String, person:PersonInfo, roleInfo:RoleInfo, customInfo:RoleInfo) extends Gadget[dom.HTMLTableRowElement] {
    def doRender() =
      tr(cls:=showCls,
      td({
        MSeq(
          person.person.displayName, 
          " -- ",
          new RolesDisplay(person.roles, person.person.oid, roleInfo, customInfo)
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
  
  class CustomRoleManager(customRoles:RoleInfo) extends Gadget[dom.HTMLDivElement] {
    val roleAdder = QGadgetRef[RxText]
    
    def createRole(name:String) = {
      val change = Seq(EditFunctions.ChangePropertyValue(Editing.propPath(std.basic.displayNameProp), List(name)))
      
      for {
        role <- Client[EditFunctions].create(std.security.customRoleModel, change).call()
      }
        // For now, we're just going to reload, instead of trying to do anything clever:
        yield PageManager.reload()
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
        new ButtonGadget(ButtonGadget.Warning, "Add Role", disabled := Rx { roleAdder.map(_.length == 0).getOrElse(true) }) ({ () =>
          createRole(roleAdder.get.text())
        })
      )
  }
  
  def pageContent = for {
    securityInfo <- Client[SecurityFunctions].getSecurityInfo().call()
    (roles, custom) <- Client[SecurityFunctions].getRoles().call()
    inviteEditInfo <- Client[EditFunctions].getOnePropertyEditor(DataAccess.space.get.oid, std.security.inviteTextProp).call()
    awaitingValidation = (DataAccess.request.userLevel == PendingUser)
    roleMap = makeRoleMap(roles)
    customMap = makeRoleMap(noRole +: custom)
    (members, invitees) <- Client[SecurityFunctions].getMembers().call()
    
    pageTitle = msg("pageTitle", ("spaceName" -> space.displayName))
    
    guts =
      div(
        h2(pageTitle),
        p("This page allows you to invite people into this Space, and manage what roles they play in it"),
          
        ul(cls:="nav nav-tabs", role:="tablist",
          li(role:="presentation", if (initTab == Tab.Invite) cls:="active", a(href:="#sendInvitations", role:="tab", "data-toggle".attr:="tab", "Invites")),
          li(role:="presentation", if (initTab == Tab.Members) cls:="active", a(href:="#members", role:="tab", "data-toggle".attr:="tab", "Members")),
          if (isAdvanced)
            li(role:="presentation", if (initTab == Tab.CustomRoles) cls:="active", a(href:="#custom", role:="tab", "data-toggle".attr:="tab", "Roles"))
        ),
          
        div(cls:="tab-content",
          section(role:="tabpanel", tabCls(Tab.Invite), id:="sendInvitations",
            h3("Send Invitations to Join this Space"),
            
            p("""Use this form to invite people to become Members of this Space. The Invitation Message may contain the usual Querki Text formatting,
            |including [[]]-style expressions; however, links may not yet work quite the way you expect.""".stripMargin),
            
          p("""Specify invitees by email address. Note that your current invitations are listed below. While it is acceptable to retry once or
          |twice, doing so excessively is frowned upon, as is sending unwelcome invitations.""".stripMargin, br(), 
            b("Either of these is considered spam, and is grounds for removal from Querki.")),
            
            p(s"""Invitations will come from "${securityInfo.fromEmail}". If your invitations are getting spam-filtered, tell your invitees
            |to add that address to their Contacts.""".stripMargin),
            
            p("""Invitations will have your email address included as the Reply-To, so if the invitees write back, it will go directly to you.
            |(Eventually, we may have replies come to you via Querki, but for now, keep in mind that your invitees will see your email address.)""".stripMargin),
            
            new RawDiv(inviteEditInfo.editor),
            
            div(cls:="control-group",
              label(cls:="control-label", "Who to Invite by email (enter email addresses, comma-separated)"),
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
                new RolesDisplay(securityInfo.defaultRoles, DataAccess.space.get.oid, roleMap, customMap)
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
                  flash(true, msg("notAllowedYet"), " ", UserAccess.resendActivationButton)
                }
              )
            ),
             
            h3("Outstanding Invitations"),
            p("The following invitations have been sent but not yet accepted."),
            
            table(cls:="table table-hover",
              tbody(
                for (member <- invitees.sortBy(_.person.displayName)) 
                  yield new PersonDisplay("warning", member, roleMap, customMap)
              )
            )
          ),
        
          section(role:="tabpanel", tabCls(Tab.Members), id:="members",
            h3("Members"),
            p("The following people are members of this Space. Click on a member's Role in order to change it."),
            
            table(cls:="table table-hover",
              tbody(
                for (member <- members) 
                  yield new PersonDisplay("info", member, roleMap, customMap)
              )
            )
          ),
        
          if (isAdvanced)
            section(role:="tabpanel", tabCls(Tab.CustomRoles), id:="custom",
              h3("Custom Roles"),
              p(b("Advanced: "),
                """You can define special custom Roles for your Space, if you need more control. For the moment, you
                  |can only use these Roles in the fine-grained permission system (that is, using them for permissions
                  |such as Who Can Edit); in the future, we will allow you to define Space-wide permissions for people
                  |with these Roles. Note that, for now, you can only add up to one of these Roles per Member.""".stripMargin),
              new CustomRoleManager(customMap)
            ) 
        )
      )
  }
    yield PageContents(pageTitle, guts)
}

object SharingPage {
  sealed trait Tab extends EnumEntry
  object Tab extends Enum[Tab] {
    val values = findValues
    
    case object Invite extends Tab
    case object Members extends Tab
    case object CustomRoles extends Tab
  }
}
