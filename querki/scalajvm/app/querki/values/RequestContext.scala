package querki.values

import models.OID
import models.UnknownOID
import models.Thing

import querki.ecology._

import querki.identity.{Identity, IdentityId, User}

import querki.ui.UIRenderer

/**
 * This is information about the current Request, which gets sent to the back end.
 * 
 * Note that this is highly incomplete. That's intentional. PlayRequestContext contains
 * everything, the full soup-to-nuts information about the request, but that's bulky and
 * not necessarily serializable. So this is just the key bits that are actually used by
 * the back end.
 * 
 * At this point, this is almost *too* stripped-down. Think carefully about whether anything
 * should be added to or removed from this structure.
 * 
 * @param requester The validated User who is asking for this page, if any.
 */
case class RequestContext(
    val requester:Option[User], 
    val ownerId:IdentityId, 
    val numNotifications:Int = 0)
{
  def requesterOrAnon = requester getOrElse User.Anonymous
  def requesterOID = requester map (_.id) getOrElse UnknownOID  
  def ownerHandle(implicit state:SpaceState) = state.ownerHandle
  def ownerName(implicit state:SpaceState) = state.ownerName
  
  def isOwner = requesterOrAnon.hasIdentity(ownerId)
}
