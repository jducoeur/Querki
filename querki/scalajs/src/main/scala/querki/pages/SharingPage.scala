package querki.pages

import scalatags.JsDom.all._
import scalatags.JsDom.tags2.section
import autowire._

import querki.globals._

import querki.api._
import querki.data.ThingInfo

class SharingPage(implicit e:Ecology) extends Page(e) with EcologyMember {
  
  lazy val Client = interface[querki.client.Client]
  
  lazy val space = DataAccess.space.get
  
  case class RoleInfo(map:Map[TID, ThingInfo], default:ThingInfo)
  def makeRoleMap(roles:Seq[ThingInfo]) = RoleInfo(Map(roles.map(role => (role.oid -> role)):_*), roles.head)
  
  def renderPerson(person:PersonInfo, roleInfo:RoleInfo) = {
    tr(cls:="info",
      td({
        val role = person.roles.headOption.map(roleInfo.map(_)).getOrElse(roleInfo.default)
        MSeq(
          person.person.displayName, 
          " -- ",
          span(cls:="_chooseRole label label-info",
            role.displayName
          )
        )
      })
    )
  }
  
  def pageContent = for {
    roles <- Client[SecurityFunctions].getRoles().call()
    roleMap = makeRoleMap(roles)
    (members, invitees) <- Client[SecurityFunctions].getMembers().call()
    guts =
      div(
        section(id:="sendInvitations"),
        
        section(id:="invitees"),
        
        section(id:="members",
          h2("Members"),
          p("The following people are members of this Space. Click on a member's Role in order to change it."),
          
          table(cls:="table table-hover",
            tbody(
              for (member <- members) 
                yield renderPerson(member, roleMap)
            )
          )
        )
      )
  }
    yield PageContents(s"Manage Sharing for ${space.displayName}", guts)
}
