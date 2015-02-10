package querki.api

import querki.data._

trait SecurityFunctions {
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
