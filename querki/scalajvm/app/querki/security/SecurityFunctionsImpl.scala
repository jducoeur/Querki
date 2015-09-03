package querki.security

import scala.concurrent.Future

import models.{AsOID, Thing, ThingId}
import querki.api._
import querki.data._
import querki.globals._
import querki.identity.InvitationResult
import querki.spaces.messages.{Archived, ArchiveSpace}

import SecurityFunctions._

class SecurityFunctionsImpl(info:AutowireParams)(implicit e:Ecology) extends SpaceApiImpl(info, e) with SecurityFunctions {
  
  implicit val s = state
  
  lazy val AccessControl = interface[AccessControl]
  lazy val ClientApi = interface[querki.api.ClientApi]
  lazy val Email = interface[querki.email.Email]
  lazy val Person = interface[querki.identity.Person]
  lazy val Roles = interface[Roles]
  lazy val SpaceOps = interface[querki.spaces.SpaceOps]
  
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
  
  def getRoles():Future[Seq[ThingInfo]] = {
    val roles = Roles.allRoles(state)
    Future.sequence(roles.map(ClientApi.thingInfo(_, rc)))
  }
  
  def getMembers():Future[(Seq[PersonInfo], Seq[PersonInfo])] = {
    
    def toPersonInfo(person:Thing):Future[PersonInfo] = {
      ClientApi.thingInfo(person, rc).map(PersonInfo(_, AccessControl.personRoles(person).map(role => oid2tid(role.id))))
    }
    
    for {
      members <- Future.sequence(Person.members(state).toSeq.map(toPersonInfo(_)))
      invitees <- Future.sequence(Person.invitees(state).toSeq.map(toPersonInfo(_)))
    }
      yield (members, invitees)
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
  
  def archiveThisSpace():Future[Boolean] = {
    if (!rc.isOwner && !rc.requesterOrAnon.isAdmin)
      throw new NotAllowedException()
    
    SpaceOps.spaceManager.request(ArchiveSpace(state.id)) map {
      case Archived => {
        // Have the troupe self-destruct on the way out, since this Space is no longer valid:
        spaceRouter ! querki.util.Shutdown
        true
      }
      case _ => throw new CanNotArchiveException()
    }
  }
}
