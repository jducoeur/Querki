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
  lazy val Publication = interface[querki.publication.Publication]
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

  def toPersonInfo(person:Thing):Future[PersonInfo] = {
    ClientApi.thingInfo(person, rc).map(PersonInfo(_, AccessControl.personRoles(person).map(role => oid2tid(role.id))))
  }

  def getMembers():Future[(Seq[PersonInfo], Seq[PersonInfo])] = {
    for {
      members <- Future.sequence(Person.members(state).toSeq.map(toPersonInfo(_)))
      invitees <- Future.sequence(Person.invitees(state).toSeq.map(toPersonInfo(_)))
    }
      yield (members, invitees)
  }

  def getMyInfo(): Future[Option[PersonInfo]] = {
    Person.localPerson(user)
      .map(personThing => toPersonInfo(personThing).map(Some(_)))
      .getOrElse(Future.successful(None))
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
    val msg = SpaceSubsystemRequest(user, state.id, InviteRequest(rc, inviteeEmails, collabs))
    for {
      InvitationResult(invited, alreadyInvited) <- spaceRouter ? msg
    }
      yield InviteResponse(invited, alreadyInvited)
  }

  def removeFromSpace(peopleTids: Seq[TID]): Future[Boolean] = {
    // This might eventually get its own permission, but for now I'm content to limit this to admins:
    if (!AccessControl.isManager(rc.requesterOrAnon, state))
      throw NotAllowedException()

    val peopleIds = for {
      tid <- peopleTids
      thingId = ThingId(tid.underlying)
      AsOID(oid) = thingId
    }
      yield oid

    val msg = SpaceSubsystemRequest(user, state.id, RemoveMembers(rc, peopleIds))
    (spaceRouter ? msg).map { _ => true}
  }

  def archiveThisSpace():Future[Boolean] = {
    // This really-and-for-true is only allowed for the Owner, not a Manager:
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
  
  private def perm2Api(perm:Property[OID,_]):PermInfo = {
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
  
  def getOnePerm(id:TID):Future[PermInfo] = withThing(id) { thing =>
    thing match {
      case prop:Property[_,_] => {
        val linkProp = prop.confirmType(Core.LinkType).get
        fut(perm2Api(linkProp))
      }
    }
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
  
        // TBD, but for now these can just be Manager Permissions. In the long run,
        // these might be "advanced permissions" or some such. See QI.7w4gaqs
//        Apps.CanManipulateAppsPerm,
//        Apps.CanUseAsAppPerm,
        
        Publication.CanPublishPermission,
        Roles.CanManageSecurityPerm
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
  
  def makeSharedLinkInfo(link: Thing): Future[SharedLinkInfo] = {
    ClientApi.thingInfo(link, rc).map { thingInfo =>
      SharedLinkInfo(
        thingInfo, 
        link.firstOpt(Roles.InviteRoleLink).getOrElse(UnknownOID).toTOID, 
        link.ifSet(Roles.InviteRequiresMembership), 
        link.ifSet(Roles.IsOpenInvitation))
    }
  }
    
  def getOneSharedLink(linkToid: TOID): Future[SharedLinkInfo] = {
    if (!AccessControl.hasPermission(Roles.CanManageSecurityPerm, state, user, state.id))
      throw new NotAllowedException()
    
    val linkId = OID.fromTOID(linkToid)
    
    val link = state.anything(linkId).getOrElse {
      throw new Exception(s"Trying to fetch unknown Shared Link $linkToid")
    }
    
    makeSharedLinkInfo(link)
  }
  
  def getSharedLinksForRole(roleTid: TOID): Future[Seq[SharedLinkInfo]] = {
    if (!AccessControl.hasPermission(Roles.CanManageSecurityPerm, state, user, state.id))
      throw new NotAllowedException()
    
    val roleId = OID.fromTOID(roleTid)
    
    val allLinks = state.children(Roles.SharedInviteModel).toList
    
    val linksForThisRole = allLinks.collect { case link =>
      for {
        linkRoleId <- link.firstOpt(Roles.InviteRoleLink)
        if (linkRoleId == roleId)
      }
        yield link
    }.flatten
    
    Future.sequence(linksForThisRole.map(makeSharedLinkInfo))
  }
  
  def getSharedLinkURL(linkId: TOID): Future[String] = {
    if (!AccessControl.hasPermission(Roles.CanManageSecurityPerm, state, user, state.id))
      throw new NotAllowedException()
    
    // TODO: yes, this is ugly with the hard .get. But this is a real error, that suggests
    // something bad is going on, if it fails.
    val link = state.anything(OID.fromTOID(linkId)).get
    
    if (!link.ifSet(Roles.IsOpenInvitation))
      // Again, this shouldn't happen -- somebody is trying to get the URL of a closed Shared Link.
      throw new NotAllowedException()
    
    // Okay -- at this point, we have what appears to be a legit Link, so generate the URL:
    fut(NotifyInvitations.generateShareableLink(link, state))
  }
}
