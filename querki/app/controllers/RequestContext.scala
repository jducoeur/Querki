package controllers

import play.api.mvc._
import models._

import language.implicitConversions
import identity.User

class RequestHeaderParser(request:RequestHeader) {
  def hasQueryParam(paramName:String) = request.queryString.contains(paramName)
  def queryParam(paramName:String):Seq[String] = if (hasQueryParam(paramName)) request.queryString(paramName) else Seq.empty
  def firstQueryParam(paramName:String):Option[String] = {
    val seq = queryParam(paramName)
    if (seq.isEmpty) None else Some(seq.head)
  }
  def paramIs(paramName:String, value:String) = hasQueryParam(paramName) && firstQueryParam(paramName).map(_ == value).getOrElse(false)
  def isTrue(paramName:String) = paramIs(paramName, "true")  
}

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
 * belong in controllers per se. Also, the "request" needs to be abstracted out, so that the
 * internals don't have to know about it.
 * 
 * @param requester The validated User who is asking for this page, if any.
 */
case class RequestContext(
    request:Request[AnyContent], 
    requester:Option[User], 
    ownerId:OID, 
    state:Option[SpaceState], 
    thing:Option[Thing],
    error:Option[String] = None,
    sessionUpdates:Seq[(String,String)] = Seq.empty) extends RequestHeaderParser(request) {
  def requesterOrAnon = requester getOrElse User.Anonymous
  def requesterOID = requester map (_.id) getOrElse UnknownOID  
  def ownerName = state map Application.ownerName getOrElse ""
  
  
  val chromelessName = "cl"
  def turningOn(name:String):Boolean = paramIs(name, "on")
  def turningOff(name:String):Boolean = paramIs(name, "off")
  // The chromeless param can either be set page-by-page, or be turned on or off.
  // TODO: there's probably a general mechanism fighting to break out here.
  def chromeless = {
    !turningOff(chromelessName) && 
      (hasQueryParam(chromelessName) || turningOn(chromelessName) || request.session.get(chromelessName).map(_ == "on").getOrElse(false))
  }
  
  def allSessionUpdates:Seq[(String, String)] = {
    if (turningOn(chromelessName))
      sessionUpdates :+ (chromelessName -> "on")
    else if (turningOff(chromelessName))
      sessionUpdates :+ (chromelessName -> "off")
    else
      sessionUpdates
  }
  
  val propStrName = "prop"
  def hasProp = hasQueryParam(propStrName)
  def propStr = firstQueryParam(propStrName)
  // If there was a property specified as a query parameter, that is the property we should
  // evaluate and render. This returns that property, if there is one:
  def prop:Option[Property[_,_]] = {
    // TBD: I should be able to write this as a for comprehension, but I'm doing
    // something wrong in the syntax. Fix it:
    state.flatMap(space => propStr.flatMap(id => space.prop(ThingId(id))))
  }
  
  def updateSession(result:Result):Result = {
    val updates = allSessionUpdates
    if (updates.isEmpty)
      result
    else {
      val newSession = (request.session /: updates) ((sess, update) => sess + (update._1 -> update._2))
      result.withSession(newSession)
    }
  }
}
object RequestContext {
  implicit def rc2Space(rc:RequestContext) = rc.state
}