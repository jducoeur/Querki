package querki.security

import scala.concurrent.Future

import models.{AsOID, Thing, ThingId}
import querki.api._
import querki.data._
import querki.globals._
import querki.identity.InvitationResult
import querki.session.{AutowireApiImpl, AutowireParams}

import SecurityFunctions._

class SecurityFunctionsImpl(info:AutowireParams)(implicit e:Ecology) extends AutowireApiImpl(info, e) with SecurityFunctions {
  
  implicit val s = state
  
  lazy val AccessControl = interface[AccessControl]
  lazy val ClientApi = interface[querki.api.ClientApi]
  lazy val Email = interface[querki.email.Email]
  lazy val Person = interface[querki.identity.Person]
  lazy val Roles = interface[Roles]
  
  def doRoute(req:Request):Future[String] = route[SecurityFunctions](this)(req)
  
  implicit def oid2tid(oid:OID):TID = TID(oid.toThingId.toString)
  
  def getSecurityInfo():SpaceSecurityInfo = {
    val currentDefaultOpt:Option[Thing] = for {
      rolesPV <- state.getPropOpt(AccessControl.PersonRolesProp)
      roleId <- rolesPV.firstOpt
      role <- state.anything(roleId)
    }
      yield role
      
    val currentDefault = currentDefaultOpt.getOrElse(Roles.BasicMemberRole)    
    
    SpaceSecurityInfo(Email.from, currentDefault.id)
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
  
  lazy val maxMembers = Config.getInt("querki.public.maxMembersPerSpace", 100)
  
  def invite(emailStrs:Seq[String], collabTids:Seq[TID]):Future[InviteResponse] = {
	  val nCurrentMembers = Person.people(state).size
    val inviteeEmails = emailStrs.map(querki.email.EmailAddress(_))
    val collabs = for {
      tid <- collabTids
      thingId = ThingId(tid.underlying)
      AsOID(oid) = thingId
    }
	  yield oid
    
    if (!rc.requesterOrAnon.isAdmin && (nCurrentMembers + inviteeEmails.size + collabs.size) > maxMembers)
      throw new MaxMembersPerSpaceException(maxMembers)
	
    Person.inviteMembers(rc, inviteeEmails, collabs, state).map { case InvitationResult(invited, alreadyInvited) =>
      InviteResponse(invited, alreadyInvited)
    }
  }
}
