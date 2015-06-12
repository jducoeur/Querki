package querki.values

import models.OID
import models.UnknownOID
import models.Thing

import querki.ecology._

import querki.identity.{Identity, IdentityId, User}

import querki.ui.UIRenderer

/**
 * As Querki gets more complex, we're passing larger and larger bundles of information around.
 * So instead of trying to do that all in separate parameters, we're taking all the common
 * parts and building up this RequestContext object.
 * 
 * This initially gets filled by the request itself; as things process, this may get replaced
 * by more-filled versions.
 * 
 * This object is a bit squishy semantically, but the high concept is that it should encapsulate
 * all the data that we *typically* pass around into *most* page renders.
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
