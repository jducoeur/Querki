package querki.security

import querki.globals._

import models.Thing
import querki.api._
import querki.data._
import querki.session.{AutowireApiImpl, AutowireParams}

class SecurityFunctionsImpl(info:AutowireParams)(implicit e:Ecology) extends AutowireApiImpl(info, e) with SecurityFunctions {
  
  implicit val s = state
  
  lazy val AccessControl = interface[AccessControl]
  lazy val ClientApi = interface[querki.api.ClientApi]
  lazy val Email = interface[querki.email.Email]
  lazy val Person = interface[querki.identity.Person]
  lazy val Roles = interface[Roles]
  
  implicit def oid2tid(oid:OID):TID = TID(oid.toThingId.toString)
  
  def getSecurityInfo():SpaceSecurityInfo = {
    SpaceSecurityInfo(Email.from)
  }
  
  def getRoles():Seq[ThingInfo] = {
    val roles = Roles.allRoles(info.state)
    roles.map(ClientApi.thingInfo(_, rc))
  }
  
  def getMembers():(Seq[PersonInfo], Seq[PersonInfo]) = {
    
    def toPersonInfo(person:Thing):PersonInfo = {
      PersonInfo(ClientApi.thingInfo(person, rc), AccessControl.personRoles(person).map(role => oid2tid(role.id)))
    }
    
    (Person.members(state).toSeq.map(toPersonInfo(_)), Person.invitees(state).toSeq.map(toPersonInfo(_)))
  }
}
