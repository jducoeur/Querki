package querki.security

import scalatags.JsDom.all._
import autowire._
import enumeratum._

import querki.globals._

import querki.data.ThingInfo
import querki.display.TabSetGadget
import querki.pages._

class SharingPage(params:ParamMap)(implicit val ecology:Ecology) extends Page("sharing") {
  
  lazy val Client = interface[querki.client.Client]
  lazy val SkillLevel = interface[querki.identity.skilllevel.SkillLevel]
  
  lazy val space = DataAccess.space.get
  
  lazy val isAdvanced = SkillLevel.current == SkillLevel.AdvancedComplexity
  
  def makeRoleMap(roles:Seq[ThingInfo]) = RoleInfo(Map(roles.map(role => (role.oid -> role)):_*), roles)
  
  def pageContent = {
    for {
      (roles, custom) <- Client[SecurityFunctions].getRoles().call()
      roleMap = makeRoleMap(roles)
      customMap = makeRoleMap(custom)
      (members, invitees) <- Client[SecurityFunctions].getMembers().call()
      
      pageTitle = msg("pageTitle", ("spaceName" -> space.displayName))
      
      tabList = 
        List(
          new InvitationTab(roleMap, customMap, invitees, this),
          new MembersTab(roleMap, customMap, members, this)) ++ 
        (if (isAdvanced)
          List(new CustomRolesTab(customMap, this))
        else
          List())
      tabSet = new TabSetGadget(params, tabList)
      tabSetContent <- tabSet.tabSetContent
      
      guts =
        div(
          h2(pageTitle),
          p("This page allows you to invite people into this Space, and manage what roles they play in it"),
          p(
            b("To create an invitation that you can share with multiple people: "),
            """Create or select a Custom Role below; make sure that it lets the recipients do what you want them to
                |be able to do; then create an Invitation from that Role.""".stripMargin
          ),
          tabSetContent
        )
    }
      yield PageContents(pageTitle, guts)
  }
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
