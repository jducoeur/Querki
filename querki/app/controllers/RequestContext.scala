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
 * TODO: now that we've gotten into serious questions of rendering, this structure is being
 * passed around more and more. It is getting deep into models and ql, so it probably doesn't
 * belong in controllers per se.
 * 
 * @param requester The validated User who is asking for this page, if any.
 */
case class RequestContext(
    request:Request[AnyContent], 
    requester:Option[User], 
    ownerId:OID, 
    state:Option[SpaceState], 
    thing:Option[Thing],
    error:Option[String] = None) {
  def requesterOID = requester map (_.id) getOrElse UnknownOID  
  def ownerName = state map Application.ownerName getOrElse ""
  
  def hasQueryParam(paramName:String) = request.queryString.contains(paramName)
  def queryParam(paramName:String):Seq[String] = request.queryString(paramName)
  def firstQueryParam(paramName:String):Option[String] = {
    queryParam(paramName) match {
      case first :: rest => Some(first)
      case _ => None
    }
  }
  
  def chromeless = hasQueryParam("cl")
}

object RequestContext {
  implicit def rc2Space(rc:RequestContext) = rc.state
}