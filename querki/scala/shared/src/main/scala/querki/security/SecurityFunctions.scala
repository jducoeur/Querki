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
   * Fetches the Instance Permissions for the specified Space or Model. Will create the
   * Instance Permissions Thing if it doesn't already exist.
   */
  def instancePermsFor(thing:TID):Future[InstancePermissions]
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
  case object SecPublic extends SecurityLevel
  case object SecMembers extends SecurityLevel
  case object SecOwner extends SecurityLevel
  case object SecCustom extends SecurityLevel
  
  case class InstancePerm(permId:TID, currently:SecurityLevel)
  case class InstancePermissions(permThing:ThingInfo, perms:Seq[InstancePerm])
}
