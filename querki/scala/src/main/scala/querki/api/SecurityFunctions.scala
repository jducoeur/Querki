package querki.api

import querki.data._

trait SecurityFunctions {
  /**
   * Gets general information for security management.
   */
  def getSecurityInfo():SpaceSecurityInfo

  /**
   * Fetch all of the Roles known to this Space, in their "usual" display order.
   */
  def getRoles():Seq[ThingInfo]
  
  /**
   * Fetch all of the members and invitees of this Space.
   */
  def getMembers():(Seq[PersonInfo], Seq[PersonInfo])
}

case class PersonInfo(person:ThingInfo, roles:Seq[TID])

/**
 * Catch-all for information about security.
 *
 * IMPORTANT: think carefully about anything that goes in here! It is not necessarily intended
 * as a high-security mechanism, and certainly must NEVER include anything that routine end
 * users aren't allowed to see.
 */
case class SpaceSecurityInfo(fromEmail:String)
