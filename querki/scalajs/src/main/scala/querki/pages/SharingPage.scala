package querki.pages

import org.scalajs.dom
import org.scalajs.jquery._
import scalatags.JsDom.all._
import scalatags.JsDom.tags2.section
import autowire._
import rx._

import querki.globals._

import querki.api._
import querki.data.ThingInfo
import querki.display.Gadget
import querki.display.input.InputGadget
import querki.display.rx.RxTextFrag

class SharingPage(implicit e:Ecology) extends Page(e) with EcologyMember {
  
  lazy val Client = interface[querki.client.Client]
  lazy val Editing = interface[querki.editing.Editing]
  
  lazy val space = DataAccess.space.get
  
  case class RoleInfo(map:Map[TID, ThingInfo], roles:Seq[ThingInfo]) {
    def default = roles.head
  }
  def makeRoleMap(roles:Seq[ThingInfo]) = RoleInfo(Map(roles.map(role => (role.oid -> role)):_*), roles)
  
  class PersonDisplay(showCls:String, person:PersonInfo, roleInfo:RoleInfo, std:StandardThings) extends Gadget[dom.HTMLTableRowElement] {
    
    class RoleDisplay extends InputGadget[dom.HTMLSpanElement](ecology) {
      val role = Var(person.roles.headOption.map(roleInfo.map(_)).getOrElse(roleInfo.default))
      val roleName = Rx(role().displayName)
      
      override lazy val thingId = person.person.oid
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
          data("personid"):=person.person.oid.underlying,
          new RxTextFrag(roleName)
        )
        
      lazy val selector = (new RoleSelector).render
        
      class RoleSelector extends Gadget[dom.HTMLSelectElement] {
        override def onCreate(e:dom.HTMLSelectElement) = {
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
    
    def doRender() =
      tr(cls:=showCls,
	    td({
	      MSeq(
	        person.person.displayName, 
	        " -- ",
	        new RoleDisplay
	      )
	    })
	  )
  }
  
  def pageContent = for {
    std <- DataAccess.standardThings
    roles <- Client[SecurityFunctions].getRoles().call()
    roleMap = makeRoleMap(roles)
    (members, invitees) <- Client[SecurityFunctions].getMembers().call()
    guts =
      div(
        section(id:="sendInvitations"),
        
        section(id:="invitees",
          h2("Outstanding Invitations"),
          p("The following invitations have been sent but not yet accepted."),
          
          table(cls:="table table-hover",
            tbody(
              for (member <- invitees) 
                yield new PersonDisplay("warning", member, roleMap, std)
            )
          )          
        ),
        
        section(id:="members",
          h2("Members"),
          p("The following people are members of this Space. Click on a member's Role in order to change it."),
          
          table(cls:="table table-hover",
            tbody(
              for (member <- members) 
                yield new PersonDisplay("info", member, roleMap, std)
            )
          )
        )
      )
  }
    yield PageContents(s"Manage Sharing for ${space.displayName}", guts)
}
