package querki.security

import scala.concurrent.Future

import models.{AsOID, Kind, Thing, ThingId}
import querki.api._
import querki.data._
import querki.globals._
import querki.identity.InvitationResult
import querki.spaces.messages._

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
    val currentDefaultOpt:Option[Seq[OID]] = 
      state.getPropOpt(AccessControl.PersonRolesProp).map(_.rawList)
      
    val currentDefault = currentDefaultOpt.getOrElse(List(Roles.BasicMemberRole.id))    
    
    SpaceSecurityInfo(Email.from, currentDefault.map(oid2tid(_)))
  }
  
  def getRoles():Future[(Seq[ThingInfo], Seq[ThingInfo])] = {
    val (std, custom) = Roles.allRoles(state)
    val stdFut = Future.sequence(std.map(ClientApi.thingInfo(_, rc)))
    val customFut = Future.sequence(custom.map(ClientApi.thingInfo(_, rc)))
    Future.sequence(Seq(stdFut, customFut)).map {
      case head :: tail :: _ => (head, tail)
    }
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

    // We need to route the request through the SpaceMembersActor, for security reasons: the invitation
    // itself needs to be done with elevated security, so that it has access to peoples' email addresses.
    val msg = SpaceMembersMessage(user, state.id, InviteRequest(rc, inviteeEmails, collabs))
    for {
      InvitationResult(invited, alreadyInvited) <- spaceRouter ? msg
    }
      yield InviteResponse(invited, alreadyInvited)
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
  
  def instancePermsFor(thing:TID):Future[InstancePermissions] = withThing(thing) { thing =>
    val permThingOpt = for {
      oid <- thing.getFirstOpt(AccessControl.InstancePermissionsProp)(state)
      t <- state.anything(oid)
    }
      yield t

    // Either we have the Instance Permissions Thing, or we create it:
    val permThingFut:Future[(Thing, SpaceState)] = 
      permThingOpt.map(t => Future.successful((t, state))).getOrElse {
        val createMsg = CreateThing(user, state.id, Kind.Thing, MOIDs.InstancePermissionsModelOID, Thing.emptyProps)
        for {
          // Create the Permissions Thing:
          ThingFound(permThingId, _) <- spaceRouter ? createMsg
          // And point the main Thing to it:
          changeMsg = ChangeProps(user, state.id, thing.id, Map(AccessControl.InstancePermissionsProp(permThingId)))
          ThingFound(_, newState) <- spaceRouter ? changeMsg
          t = newState.anything(permThingId).get
        }
          yield (t, newState)
      }
      
    // thingInfo() also produces a Future, so comprehension time:
    for {
      (t, s) <- permThingFut
      thingInfo <- ClientApi.thingInfo(t, rc)(s)
    }
      // TODO: summarize the actual permissions
      yield InstancePermissions(thingInfo, Seq.empty)
  }
}
