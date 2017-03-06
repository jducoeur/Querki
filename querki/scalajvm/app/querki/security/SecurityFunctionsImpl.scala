package querki.security

import scala.concurrent.Future

import models._
import querki.api._
import querki.data._
import querki.globals._
import querki.identity.InvitationResult
import querki.spaces._
import querki.spaces.messages._

import SecurityFunctions._

class SecurityFunctionsImpl(info:AutowireParams)(implicit e:Ecology) extends SpaceApiImpl(info, e) with SecurityFunctions {
  
  implicit val s = state
  
  lazy val AccessControl = interface[AccessControl]
  lazy val Apps = interface[querki.apps.Apps]
  lazy val Basic = interface[querki.basic.Basic]
  lazy val ClientApi = interface[querki.api.ClientApi]
  lazy val Conventions = interface[querki.conventions.Conventions]
  lazy val Conversations = interface[querki.conversations.Conversations]
  lazy val Core = interface[querki.core.Core]
  lazy val Email = interface[querki.email.Email]
  lazy val NotifyInvitations = interface[querki.identity.NotifyInvitations]
  lazy val Person = interface[querki.identity.Person]
  lazy val Roles = interface[Roles]
  lazy val SpaceOps = interface[querki.spaces.SpaceOps]
  lazy val UserValues = interface[querki.uservalues.UserValues]
  
  lazy val InstancePermissionsProp = AccessControl.InstancePermissionsProp
  
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
    
    SpaceOps.spaceManager.request(ChangeSpaceStatus(state.id, StatusArchived)) map {
      case StatusChanged => {
        // Have the troupe self-destruct on the way out, since this Space is no longer valid:
        spaceRouter ! querki.util.Shutdown
        true
      }
      case _ => throw new CanNotArchiveException()
    }
  }
  
  lazy val allPerms = AccessControl.allPermissions(state)
  lazy val allPermIds = allPerms.map(_.id).toSet
  lazy val permMap = Map(allPerms.toSeq.map(p => (p.id, p)):_*)
  
  /**
   * Note that, at this level, we're just providing a summary. In the "Custom" case, the Client asks for
   * the actual Editor.
   */
  def translatePerm(permId:OID, vs:List[OID]):ThingPerm = {
    val level:SecurityLevel =
      if (vs.isEmpty || (vs.length == 1 && vs.contains(querki.security.MOIDs.OwnerTagOID)))
        SecurityOwner
      else if (vs.contains(querki.security.MOIDs.PublicTagOID))
        SecurityPublic
      else if (vs.contains(querki.security.MOIDs.MembersTagOID))
        SecurityMembers
      else
        SecurityCustom
        
    ThingPerm(permId, level)
  }
  
  def permsFor(thing:Thing, state:SpaceState):Seq[ThingPerm] = {
    for {
      pair <- thing.props.toSeq
      if (allPermIds.contains(pair._1))
      vs = pair._2.rawList(Core.LinkType)
    }
      yield translatePerm(pair._1, vs)
  }
  
  def permsFor(thing:TID):Future[ThingPermissions] = withThing(thing) { thing =>
    for {
      // Go to the Space Plugin for the InstancePermissions, because it may create that object:
      ThingFound(permsId, newState) <- spaceRouter ? SpacePluginMsg(user, state.id, GetInstancePermissionsObject(thing.id))
      permThingOpt = 
        if (permsId == thing.id)
          None
        else
          newState.anything(permsId)
      thingInfoOpt <- futOpt(permThingOpt.map { ClientApi.thingInfo(_, rc)(newState) }.orElse(None))
    }
      yield ThingPermissions(permsFor(thing, s), thingInfoOpt, permThingOpt.map(permsFor(_, newState)).getOrElse(Seq.empty))
  }
  
  private def perm2Api(perm:Property[OID,OID]):PermInfo = {
    PermInfo(
      perm.id,
      perm.getPropAll(Core.NameProp).head,
      perm.ifSet(AccessControl.IsInstancePermissionProp),
      // TODO: this is a bit suspicious -- in principle, Summary should get fully processed:
      perm.getFirstOpt(Conventions.PropSummary).map(_.text).getOrElse(""),
      perm.ifSet(AccessControl.PublicAllowedProp),
      translatePerm(perm.id, perm.getPropAll(AccessControl.DefaultPermissionProp)).currently,
      perm.getPropAll(AccessControl.PermAppliesTo).map(oid2tid(_))
    )
  }
  
  def getAllPerms():Future[Seq[PermInfo]] = {
    // Note that this list defines the canonical editing order for permissions:
    val perms = 
      Seq(
        AccessControl.CanReadProp,
        AccessControl.CanEditProp,
        AccessControl.CanCreateProp,
        AccessControl.CanDesignPerm,
        
        Conversations.CanComment,
        Conversations.CanReadComments,
        
        Roles.CanExplorePerm,
        
        UserValues.UserValuePermission,
        
        Apps.CanManipulateAppsPerm,
        Apps.CanUseAsAppPerm
      )
    val infos = perms.map(perm2Api(_))
    fut(infos)
  }
  
  def getLinkPermChoices():Future[Seq[LinkPermsChoice]] = {
    val choices:Seq[LinkPermsChoice] = Seq(
      LinkPermsChoice("View Only", Seq(
          AccessControl.CanReadProp,
          Conversations.CanReadComments
        )
      ),
      
      LinkPermsChoice("Fill in Polls", Seq(
          AccessControl.CanReadProp,
          Conversations.CanReadComments,
          UserValues.UserValuePermission
        )
      ),
      
      LinkPermsChoice("Comment", Seq(
          AccessControl.CanReadProp,
          Conversations.CanReadComments,
          UserValues.UserValuePermission,
          Conversations.CanComment
        )
      ),
      
      LinkPermsChoice("Edit", Seq(
          AccessControl.CanReadProp,
          Conversations.CanReadComments,
          UserValues.UserValuePermission,
          Conversations.CanComment,
          AccessControl.CanEditProp,
          AccessControl.CanCreateProp
        )
      ),
      
      // This option is intended for advanced use: you can take the resulting Role and
      // apply it manually to specific Things.
      LinkPermsChoice("Nothing", Seq())
    )
    
    fut(choices)
  }
  
  def makeShareableLink(name:String, permTIDs:Seq[TOID]):Future[String] = {
    if (rc.isOwner)
      throw new NotAllowedException()
    
    val makeRoleMsg = CreateThing(user, state.id, Kind.Thing, RolesMOIDs.CustomRoleModelOID, 
      toProps(
        Basic.DisplayNameProp(name),
        AccessControl.RolePermissionsProp(permTIDs.map(OID.fromTOID(_)):_*),
        Roles.IsOpenInvitation(true)))
        
    for {
      ThingFound(roleId, newState) <- spaceRouter ? makeRoleMsg
      url = NotifyInvitations.generateShareableLink(roleId, newState)
    }
      yield url

  }
}
