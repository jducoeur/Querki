package querki.security

import models.ThingState

import querki.ecology._
import querki.globals._
import querki.values.{QLContext, SpaceState}

object RolesMOIDs extends EcotIds(51) {
  val CommentatorOID = moid(1)
  val ContributorOID = moid(2)
  val EditorOID = moid(3)
  val ManagerOID = moid(4)
  val BasicMemberOID = moid(5)
  val CanExplorePermOID = moid(6)
  val CustomRoleModelOID = moid(7)
  val OpenInvitationOID = moid(8)
  val InviteRoleLinkOID = moid(9)
  val SharedInviteModelOID = moid(10)
  val CanManageSecurityPermOID = moid(11)
  val InviteRequiresMembershipOID = moid(12)
  val HasRoleOID = moid(13)
  val RoleMembersOID = moid(14)
}

/**
 * This Ecot simply defines the OOTB Roles for Querki. It is separated from the main AccessControlModule mainly
 * for dependency reasons: whereas AccessControl is fairly central (core of the onion), this depends on lots of stuff
 * (outer layer of the onion).
 */
class RolesEcot(e:Ecology) extends QuerkiEcot(e) with Roles with querki.core.MethodDefs {
  import RolesMOIDs._
  import querki.api.commonName
  
  val AccessControl = initRequires[AccessControl]
  val Apps = initRequires[querki.apps.Apps]
  val Basic = initRequires[querki.basic.Basic]
  val Conversations = initRequires[querki.conversations.Conversations]
  val UserValues = initRequires[querki.uservalues.UserValues]
  
  lazy val Person = interface[querki.identity.Person]
  
  def allRoles(state:SpaceState):(Seq[Thing], Seq[Thing]) = {
    (Seq(
      BasicMemberRole,
      CommentatorRole,
      ContributorRole,
      EditorRole,
      ManagerRole
     ),
     state.descendants(CustomRoleModel, false, true, false).toSeq)
  }
  
  /***********************************************
   * FUNCTIONS
   ***********************************************/
  
  lazy val HasRoleFunction = new InternalMethod(HasRoleOID,
    toProps(
      setName("_hasRole"),
      Summary("Lets you check whether the current user, or some other Person, has the specified Role"),
      Categories(SecurityTag),
      Signature(
        expected = Some(Seq(AnyType), "A Person, or anything"),
        reqs = Seq(
          ("role", LinkType, "A Role")
        ),
        opts = Seq.empty,
        returns = (YesNoType, "True iff the specified Person has the specified Role.")
      ),
      Details("""If this function receives a Person, it checks that Person. Otherwise, it checks the current
                |user. These are both useful, in different situations.""".stripMargin)))
  {
    def receivedPerson(received: QValue)(implicit state: SpaceState): Option[Thing] = {
      for {
        id <- received.firstAs(LinkType)
        thing <- state.anything(id)
        if thing.isAncestor(querki.identity.MOIDs.PersonOID)
      }
        yield thing
    }
    
    def requestingPerson(context: QLContext)(implicit state:SpaceState): Option[Thing] = {
      for {
        user <- context.request.requester
        person <- Person.localPerson(user)
      }
        yield person
    }
    
    override def qlApply(inv:Invocation):QFut = {
      implicit val state = inv.state
      val personOpt: Option[Thing] = receivedPerson(inv.context.value) orElse requestingPerson(inv.context)
      
      personOpt.map { person =>
        val personRoles = person.getPropVal(AccessControl.PersonRolesProp).rawList(LinkType)
        val qf: QFut = for {
          role <- inv.processAs("role", LinkType)
        }
          yield ExactlyOne(YesNoType(personRoles.contains(role)))
        qf
      }.getOrElse(fut(ExactlyOne(YesNoType(false))))
    }
  }

  /***********************************************
   * PERMISSIONS AND PROPERTIES
   ***********************************************/
  
  /**
   * This permission doesn't precisely belong here -- it's not specific to Roles per se -- but it's a decent place to
   * put it.
   */
  lazy val CanExplorePerm = AccessControl.definePermission(CanExplorePermOID, 
      commonName(_.roles.canExplorePerm),
      "These people are allowed to explore this Space, with functions like Search, Explore, All Things and so on. If disabled, these people will not see those features.",
      Seq(AccessControl.PublicTag, AccessControl.OwnerTag),
      Seq(AccessControl.AppliesToSpace),
      false,
      true)
      
  lazy val CanManageSecurityPerm = AccessControl.definePermission(CanManageSecurityPermOID, 
      commonName(_.roles.canManageSecurityPerm), 
      "These people are allowed to manage security -- invite people, create Roles, and so on", 
      Seq(AccessControl.OwnerTag), 
      Seq(AccessControl.AppliesToSpace), 
      false, 
      false)
      
  lazy val IsOpenInvitation = new SystemProperty(OpenInvitationOID, YesNoType, Optional,
    toProps(
      setName(commonName(_.security.isOpenInvite)),
      setInternal,
      Summary("Flag on a Shared Invite, indicating that it is still open.")))
  
  lazy val InviteRoleLink = new SystemProperty(InviteRoleLinkOID, LinkType, Optional,
    toProps(
      setName(commonName(_.security.inviteRoleLink)),
      setInternal,
      Summary("Link from a Shared Invite to the Role that recipients will receive.")))
  
  lazy val InviteRequiresMembership = new SystemProperty(InviteRequiresMembershipOID, YesNoType, Optional,
    toProps(
      setName(commonName(_.security.inviteRequiresMembership)),
      setInternal,
      Summary("When set on a Shared Invitation Link, that Link will force recipients to sign up, rather than being Guests.")))
      
  override lazy val props = Seq(
    HasRoleFunction,
      
    CanExplorePerm,
    CanManageSecurityPerm,
    IsOpenInvitation,
    InviteRoleLink,
    InviteRequiresMembership
  )

  /***********************************************
   * THINGS
   ***********************************************/

  def defineRole(id:OID, name:String, display:String, desc:String, perms:Seq[Property[OID,OID]]):ThingState = ThingState(id, systemOID, AccessControl.RoleModel,
    toProps(
      setName(name),
      Basic.DisplayNameProp(display),
      AccessControl.RolePermissionsProp(perms.map(_.id):_*),
      Categories(SecurityTag),
      Summary(desc)))
      
  lazy val basicMemberPerms = Seq.empty[Property[OID,OID]]
  lazy val BasicMemberRole =
    defineRole(BasicMemberOID, "Basic Member Role", "Basic Member",
      """Basic Member -- can read Things and Comments in this Space, but that's it.""".stripMargin,
      basicMemberPerms)
      
  lazy val commentatorPerms = Seq(AccessControl.CanReadProp, Conversations.CanReadComments, Conversations.CanComment, UserValues.UserValuePermission) ++ basicMemberPerms
  lazy val CommentatorRole =
    defineRole(CommentatorOID, "Commentator Role", "Commentator",
      """Commentator -- can read Things, leave Comments, and provide Ratings and Reviews""".stripMargin,
      commentatorPerms)
      
  lazy val contributorPerms = Seq(AccessControl.CanCreateProp, AccessControl.CanEditProp) ++ commentatorPerms
  lazy val ContributorRole =
    defineRole(ContributorOID, "Contributor Role", "Contributor",
      """Contributor -- can do everything a Commentator can, plus create and edit Instances.""".stripMargin,
      contributorPerms)
      
  lazy val editorPerms = Seq(AccessControl.CanDesignPerm, Conversations.CanModerate) ++ contributorPerms
  lazy val EditorRole =
    defineRole(EditorOID, "Editor Role", "Editor",
      """Editor / Designer -- can do everything a Contributor can, plus design Models. Once Moderation is
        |implemented, Editors will be able to moderate contributions from non-Members.""".stripMargin,
      editorPerms)
      
  // Note: we put the permissions on Manager on general principles, but in practice a Manager can do
  // *everything* an Owner can do, except functions that are specifically checked as isOwner.
  lazy val managerPerms = 
    Seq(
      Apps.CanManipulateAppsPerm, 
      CanManageSecurityPerm, 
      AccessControl.CanManageDataPerm
    ) ++ editorPerms
  lazy val ManagerRole =
    defineRole(ManagerOID, "Manager Role", "Manager",
      """Manager -- can do everything an Editor can, plus almost everything the Owner of the Space can do. You should only make
        |someone a Manager if you trust them completely, because they can do anything to the Space except to give it away or delete it.""".stripMargin,
      managerPerms)
      
  lazy val CustomRoleModel = ThingState(CustomRoleModelOID, systemOID, AccessControl.RoleModel,
    toProps(
      setName(commonName(_.security.customRoleModel)),
      Categories(SecurityTag),
      Summary("The model underlying custom user-defined Roles"),
      Core.IsModelProp(true),
      SkillLevel(SkillLevelAdvanced),
      // TODO: this is too coarse-grained and hard-coded. In principle, we should have a Who Can Manage Security
      // Permission that controls this. But we don't currently have a way to delegate from one Permission
      // (CanCreate) to another (CanManageSecurity), so we'll deal with just keeping it locked down.
      AccessControl.CanCreateProp(ManagerRole),
      AccessControl.CanEditProp(ManagerRole)))
      
  lazy val SharedInviteModel = ThingState(SharedInviteModelOID, systemOID, RootOID,
    toProps(
      setName(commonName(_.security.sharedInviteModel)),
      setInternal,
      Summary("The Thing pointer to by a Shared Invite. Still good iff _isOpenInvitation is true. Not to be used directly."),
      Core.IsModelProp(true),
      // TODO: see above comment on CustomRoleModel
      AccessControl.CanCreateProp(ManagerRole),
      AccessControl.CanEditProp(ManagerRole)))
      
  override lazy val things = Seq(
    CommentatorRole,
    ContributorRole,
    EditorRole,
    ManagerRole,
    CustomRoleModel,
    SharedInviteModel
  )
}
