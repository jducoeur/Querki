package querki.security

import scala.concurrent.Future

import querki.data._

trait SecurityFunctions {
  import SecurityFunctions._
  
  /**
   * Gets general information for security management.
   */
  def getSecurityInfo():SpaceSecurityInfo

  /**
   * Fetch all of the Roles known to this Space, in their "usual" display order.
   */
  def getRoles():Future[(Seq[ThingInfo], Seq[ThingInfo])]
  
  /**
   * Fetch all of the members and invitees of this Space.
   */
  def getMembers():Future[(Seq[PersonInfo], Seq[PersonInfo])]
  
  /**
   * Invite people to join this Space. This may be any number of invitees by email address and any
   * number of known Collaborators by Identity.
   */
  def invite(emails:Seq[String], collabs:Seq[TID]):Future[InviteResponse]
  
  /**
   * Does exactly what it sounds like: it sets this Space to archived. The Space is no
   * longer available after this, so the client should navigate away from it if it
   * receives a true response.
   * 
   * This can only be run by the owner of the Space.
   * 
   * This never actually returns false; it will throw an Exception if it can't archive.
   */
  def archiveThisSpace():Future[Boolean]
  
  /**
   * Fetches the Permissions for the specified Thing. Will create the
   * Instance Permissions Thing if it is a Space or Model and they don't already exist.
   */
  def permsFor(thing:TID):Future[ThingPermissions]
  
  /**
   * Fetches the info about a single specified permission.
   */
  def getOnePerm(id:TID):Future[PermInfo]
  
  /**
   * Fetch the details on all of the permissions that are defined in this Space. (Which are
   * mostly the System ones.)
   */
  def getAllPerms():Future[Seq[PermInfo]]
  
  /**
   * Fetch the available permission levels for Shareable Links. These should be presented to
   * the user who is creating the Link, and the actual permissions passed in to
   * makeShareableLink().
   * 
   * Note that this is deliberately a bit over-designed, to make it easier to change these
   * levels, or even add custom options, down the road. It also allows for a possible
   * fine-grained UI in the Client, which lets a power user specify the permissions more
   * precisely.
   */
  def getLinkPermChoices():Future[Seq[LinkPermsChoice]]
  
  /**
   * This creates a new Open Invitation, and returns a signed URL for getting into it.
   * 
   * IMPORTANT: this uses the coarse-grained security model, and the permissions are Space-wide.
   * This is intentional -- trying to get it to work correctly with the fine-grained model is
   * tricky at best, since that would involve adding explicit overrides to specific Things, and
   * thus overriding the expected default behavior. It would likely result in unexpected behavior
   * on the target Thing.
   * 
   * We would really *like* to be able to have Model/Instance-specific invitations, but it requires
   * enhancements to the fine-grained Security system. See QI.7w4g8ht.
   * 
   * @param name The name of the resulting Custom Role.
   * @param perms The permissions granted by this Link.
   */
  def makeShareableLink(name:String, perms:Seq[TOID]):Future[String]
}

case class PersonInfo(person:ThingInfo, roles:Seq[TID])

/**
 * Catch-all for information about security.
 *
 * IMPORTANT: think carefully about anything that goes in here! It is not necessarily intended
 * as a high-security mechanism, and certainly must NEVER include anything that routine end
 * users aren't allowed to see.
 */
case class SpaceSecurityInfo(fromEmail:String, defaultRoles:Seq[TID])

object SecurityFunctions {
  case class InviteResponse(newInvites:Seq[String], resends:Seq[String])
  
  sealed trait SecurityLevel
  case object SecurityPublic extends SecurityLevel
  case object SecurityMembers extends SecurityLevel
  case object SecurityOwner extends SecurityLevel
  case object SecurityCustom extends SecurityLevel
  // This actually isn't sent from the server -- it's implicit when nothing is sent -- but is
  // used in the client:
  case object SecurityInherited extends SecurityLevel
  
  case class ThingPerm(permId:TID, currently:SecurityLevel)
  case class ThingPermissions(perms:Seq[ThingPerm], instancePermThing:Option[ThingInfo], instancePerms:Seq[ThingPerm])
  
  case class PermInfo(
    id:TID,
    name:String,
    isInstancePerm:Boolean,
    summary:String,
    publicAllowed:Boolean,
    default:SecurityLevel,
    appliesTo:Seq[TID]
  )
  
  case class LinkPermsChoice(name:String, perms:Seq[TOID])
}
