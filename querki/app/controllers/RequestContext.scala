package controllers

import play.api.mvc._

import models._

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
    requestHeader:RequestHeader, 
    requester:Option[User], 
    ownerId:OID, 
    state:Option[SpaceState], 
    thing:Option[Thing],
    error:Option[String] = None) {
  def request = requestHeader.asInstanceOf[Request[AnyContent]]
  def requesterOID = requester map (_.id) getOrElse UnknownOID  
  def ownerName = state map Application.ownerName getOrElse ""
  
  def chromeless = request.queryString.contains("cl")
}

object RequestContext {
  implicit def rc2Space(rc:RequestContext) = rc.state
}