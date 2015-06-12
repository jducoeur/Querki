package querki.values

import models.OID
import models.UnknownOID
import models.Thing

import querki.ecology._

import querki.identity.{Identity, User}

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
 * Note that this is an abstract class. You will typically pass subclasses around. RequestContext
 * represents the abstract *concept* of a request; subclasses represent the actual ways this
 * happens.
 * 
 * @param requester The validated User who is asking for this page, if any.
 */
abstract class RequestContext(
    val requester:Option[User], 
    // Note that this is an *identity*
    val ownerId:OID, 
    val numNotifications:Int = 0)
{
  def requesterOrAnon = requester getOrElse User.Anonymous
  def requesterOID = requester map (_.id) getOrElse UnknownOID  
  def ownerHandle(implicit state:SpaceState) = state.ownerHandle
  def ownerName(implicit state:SpaceState) = state.ownerName
  
  def isOwner = requesterOrAnon.hasIdentity(ownerId)
}
