package querki.security

import models.ThingState

import querki.ecology._
import querki.values.SpaceState

private [security] object RolesMOIDs extends EcotIds(51) {
  val CommentatorOID = moid(1)
  val ContributorOID = moid(2)
  val EditorOID = moid(3)
  val ManagerOID = moid(4)
  val BasicMemberOID = moid(5)
}

/**
 * This Ecot simply defines the OOTB Roles for Querki. It is separated from the main AccessControlModule mainly
 * for dependency reasons: whereas AccessControl is fairly central (core of the onion), this depends on lots of stuff
 * (outer layer of the onion).
 */
class RolesEcot(e:Ecology) extends QuerkiEcot(e) with Roles {
  import RolesMOIDs._
  
  val AccessControl = initRequires[AccessControl]
  val Conversations = initRequires[querki.conversations.Conversations]
  val UserValues = initRequires[querki.uservalues.UserValues]
  
  def allRoles(state:SpaceState):Seq[Thing] = {
    Seq(
      BasicMemberRole,
      CommentatorRole,
      ContributorRole,
      EditorRole,
      ManagerRole
    )
  }
    
  /***********************************************
   * THINGS
   ***********************************************/

  def defineRole(id:OID, name:String, desc:String, perms:Seq[Property[OID,OID]]):ThingState = ThingState(id, systemOID, AccessControl.RoleModel,
    toProps(
      setName(name),
      AccessControl.RolePermissionsProp(perms.map(_.id):_*),
      Summary(desc)))
      
  lazy val basicMemberPerms = Seq(AccessControl.CanReadProp, Conversations.CanReadComments)
  lazy val BasicMemberRole =
    defineRole(BasicMemberOID, "Basic Member Role",
      """Basic Member -- can read Things and Comments in this Space, but that's it.""".stripMargin,
      basicMemberPerms)
      
  lazy val commentatorPerms = Seq(Conversations.CanComment, UserValues.UserValuePermission) ++ basicMemberPerms
  lazy val CommentatorRole =
    defineRole(CommentatorOID, "Commentator Role",
      """Commentator -- can read Things, leave Comments, and provide Ratings and Reviews""".stripMargin,
      commentatorPerms)
      
  lazy val contributorPerms = Seq(AccessControl.CanCreateProp, AccessControl.CanEditChildrenProp) ++ commentatorPerms
  lazy val ContributorRole =
    defineRole(ContributorOID, "Contributor Role",
      """Contributor -- can do everything a Commentator can, plus create and edit Instances.""".stripMargin,
      contributorPerms)
      
  // TODO: add the CanDesignModel permission. Editors should have this; Contributors should not.
  // TODO: once we have Moderation, add the CanModerate permission here. 
  lazy val editorPerms = Seq() ++ contributorPerms
  lazy val EditorRole =
    defineRole(EditorOID, "Editor Role",
      """Editor / Designer -- can do everything a Contributor can, plus design Models and moderate contributions from non-Members
        |
        |As of this writing, Editor doesn't do anything special, since neither of its permissions actually exist. But they
        |will be added in the not-too-distant future.""".stripMargin,
      editorPerms)
      
  lazy val managerPerms = Seq() ++ editorPerms
  lazy val ManagerRole =
    defineRole(ManagerOID, "Manager Role",
      """Manager -- can do everything an Editor can, plus almost everything the Owner of the Space can do. You should only make
        |someone a Manager if you trust them completely, because they can do anything to the Space except to give it away or delete it.
        |
        |As of this writing, the Manager role doesn't actually do anything, since its permissions don't exist yet. But they
        |are coming in the not-too-distant future. Speak up if you specifically need them.""".stripMargin,
      managerPerms)
      
  override lazy val things = Seq(
    CommentatorRole,
    ContributorRole,
    EditorRole,
    ManagerRole
  )
}
