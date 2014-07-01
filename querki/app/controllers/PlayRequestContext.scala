package controllers

import play.api.mvc._
import models._

import language.implicitConversions
import querki.ecology._
import querki.identity.User

import querki.ui.UIRenderer
import querki.values.{RequestContext, SpaceState}

trait RequestHeaderParser 
{
  val request:RequestHeader
  val sessionUpdates:Seq[(String,String)]
  val returnToHere:Boolean
    
  def hasQueryParam(paramName:String) = request.queryString.contains(paramName)
  def queryParam(paramName:String):Seq[String] = if (hasQueryParam(paramName)) request.queryString(paramName) else Seq.empty
  def firstQueryParam(paramName:String):Option[String] = {
    val seq = queryParam(paramName)
    if (seq.isEmpty) None else Some(seq.head)
  }
  def paramIs(paramName:String, value:String) = hasQueryParam(paramName) && firstQueryParam(paramName).map(_ == value).getOrElse(false)
  def isTrue(paramName:String) = paramIs(paramName, "true")  
  
  // Mechanism for returning to this request after a redirect. Add this to the sessionUpdates if you
  // will want to come back here.
  val returnToParam = "returnTo"
  def returnToHereUpdate = Map((returnToParam -> request.path))
  
  def updateSession(result:Result):Result = {
    // TODO: the "allSessionUpdates" below was mainly to support the old chromeless mechanism, which is currently
    // deprecated. We may want to reintroduce something similar eventually, but not now.
    val updates = 
      if (returnToHere)
        sessionUpdates ++ returnToHereUpdate
      else
        sessionUpdates //allSessionUpdates
    if (updates.isEmpty)
      result
    else {
      val newSession = (request.session /: updates) ((sess, update) => sess + (update._1 -> update._2))
      result.withSession(newSession)
    }
  }
}

case class SimpleRequestHeaderParser(request:RequestHeader, sessionUpdates:Seq[(String,String)], returnToHere:Boolean)
  extends RequestHeaderParser

/**
 * The Play-specific version of a RequestContext.
 * 
 * Everything that knows about HTTP specifically should go in here. This deliberately belongs up in controllers,
 * at the Play level, and should not generally be exposed anywhere else. (There is some old cruft pointing to it
 * from the lower levels -- those should be considered refactoring targets.)
 */
case class PlayRequestContext(
    request:Request[AnyContent], 
    override val requester:Option[User], 
    // Note that this is an *identity*
    override val ownerId:OID, 
    override val state:Option[SpaceState], 
    override val thing:Option[Thing],
    override val ecology:Ecology,
    error:Option[String] = None,
    sessionUpdates:Seq[(String,String)] = Seq.empty,
    redirectTo:Option[Call] = None,
    spaceIdOpt:Option[String] = None,
    reqOwnerHandle:Option[String] = None,
    override val numNotifications:Int = 0) 
  extends RequestContext(requester, ownerId, state, thing, ecology, numNotifications)
  with RequestHeaderParser
{
  def renderer:UIRenderer = interface[querki.html.HtmlRenderer]
  
  // NOTE: this may be wrong, but at the moment is the way the logic works
  val returnToHere:Boolean = false
  
  def withUpdatedState(newState:SpaceState):RequestContext = copy(state = Some(newState))
  
  def turningOn(name:String):Boolean = paramIs(name, "on")
  def turningOff(name:String):Boolean = paramIs(name, "off")
  // Major session params can generally be turned on for one display, or as a "mode" in the cookies
  def isOn(name:String) = {
    !turningOff(name) && 
      (hasQueryParam(name) || turningOn(name) || request.session.get(name).map(_ == "on").getOrElse(false))
  }
  
  def sessionCookie(name:String) = request.session.get(name)
  
  def returningToHere = copy(sessionUpdates = sessionUpdates ++ returnToHereUpdate)

  def withError(err:String) = copy(error = Some(err))
  
  def APICall:Boolean = isTrue("API")
  
  // TODO: these probably don't belong here in the long run:
  val chromelessName = "cl"
  def chromeless = isOn(chromelessName)
    
  private def updatesFor(updates:Seq[(String, String)], name:String) = {
    if (turningOn(name))
      updates :+ (name -> "on")
    else if (turningOff(name))
      updates :+ (name -> "off")
    else
      updates    
  }
  
  def allSessionUpdates:Seq[(String, String)] = {
    // TODO: rewrite this whole mechanism so that we look for each parameter that is "=on" or "=off",
    // and make those adjustments:
    val update1 = updatesFor(sessionUpdates, chromelessName)

    update1
  }
  
  /**
   * This looks for a previously-set returnToHere. If found, it redirects there; otherwise, it redirects to the
   * specified other Call.
   */
  def returnToPreviousOr(other: Call):Result = {
    val redirectOpt = sessionCookie(returnToParam)
    redirectOpt match {
      case Some(redirect) => {
        val session = request.session - returnToParam
        Results.Redirect(redirect).withSession(session)
      }
      case None => Results.Redirect(other)
    }    
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
}