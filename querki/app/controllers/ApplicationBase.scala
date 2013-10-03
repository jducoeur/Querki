package controllers

import play.api.Logger
import play.api.mvc._

import models._

import querki.identity._

class ApplicationBase extends Controller {
  
  /**
   * Standard error handler. Iff you get an error and the correct response is to redirect to
   * another page, use this. The only exception is iff you need to preserve data, and thus want
   * to simply redisplay the current page; in that case, set the error in the RequestContext before
   * constructing the page.
   */
  def doError(redirectTo:Call, errorMsg:String) = {
    // TODO: figure out a better way to do this, and make it configurable:
    try {
      throw new Exception("Got error; redirecting: " + errorMsg)
    } catch {
      case e:Throwable => Logger.info(e.toString, e)
    }
    Redirect(redirectTo).flashing("error" -> errorMsg)
  }
  
  def getUser(username:String):Option[User] = User.get(username)
  
  // DEPRECATED. Delete this once I'm sure that it is unused:
  def getUserByThingId(thingIdStr:String):OID = {
    val thingId = ThingId(thingIdStr)
    thingId match {
      case AsOID(oid) => oid
      case AsName(name) => {
        if (name.length() == 0) UnknownOID
        else {
          val userOpt = getUser(name)
          userOpt map (_.id) getOrElse UnknownOID
        }
      }
    }
  }
  
  def getIdentityByThingId(thingIdStr:String):OID = {
    val thingId = ThingId(thingIdStr)
    thingId match {
      case AsOID(oid) => oid
      case AsName(name) => {
        if (name.length() == 0) UnknownOID
        else {
          val userOpt = User.getIdentity(name)
          userOpt getOrElse UnknownOID
        }
      }
    }
  }
  
  def ownerName(state:SpaceState) = User.getName(state.owner)
  def ownerHandle(state:SpaceState) = User.getHandle(state.owner)
  
  def userFromSession(request:RequestHeader) = User.get(request)
  // Workaround to deal with the fact that Security.Authenticated has to get a non-empty
  // result in order to let things through. So if a registered user is *optional*, we need to
  // return something:
  def forceUser(request: RequestHeader) = userFromSession(request) orElse Some(User.Anonymous)

  // TODO: preserve the page request, and go there after they log in
  def onUnauthorized(request: RequestHeader) = Results.Redirect(routes.LoginController.login)

  // Fetch the User from the session, or User.Anonymous if they're not found.
  def withAuth(f: => User => Request[AnyContent] => Result):EssentialAction = {
    val handler = { user:User =>
      Action(request => f(user)(request))
    }
    // Note that forceUsername can never fail, it just returns empty string
    Security.Authenticated(forceUser, onUnauthorized)(handler)
  }
  
  // TODO: we shouldn't be calling onUnauthorized directly! Instead, we should get a passed-in
  // handler to deal with lack of authorization, because we want to do different things in the
  // AJAX vs. page-view cases.
  //
  // Note that the requireLogin flag is critical, and subtle. This call will
  // *try* to get an authenticated user, but will only *require* it iff requireLogin is set.
  // This reflects the fact that there are many more or less public pages. It is the responsibility
  // of the caller to use this flag sensibly. Note that RequestContext.requester is guaranteed to
  // be set iff requireLogin is true.
  def withUser(requireLogin:Boolean)(f: RequestContext => Result) = withAuth { user => implicit request =>
    if (requireLogin && user == User.Anonymous) {
      onUnauthorized(request)
    } else {
      // Iff requireLogin was false, we might not have a real user here, so massage it:
      val userParam = if (user == User.Anonymous) None else Some(user)
      f(RequestContext(request, userParam, UnknownOID, None, None))
    }
  }
  
  /**
   * Helper for asking the SpaceManager for info. Assumes that the process is
   * asynchronous, and buffers the incoming HTTP request accordingly.
   * 
   * @tparam A The type of the expected return message from SpaceManager. This usually names a trait;
   * the actual messages should be derived from that.
   * @param msg The message to send to the SpaceManager.
   * @param cb A partial function that takes the SpaceManager response and produces a result.
   */
  def askSpaceMgr[A](msg:SpaceMgrMsg)(cb: A => Result)(implicit m:Manifest[A]) = {
    Async {
      SpaceManager.ask[A, Result](msg)(cb)
    }
  }
  
  /**
   * Given a Space, this fetches that Space's current state before calling the core logic.
   * 
   * TBD: Why the ridiculous return signature? Because I am getting cryptic errors about withSpace
   * being recursive otherwise.
   * 
   * TODO: this is fundamentally broken at the moment. withUser() is potentially long-running -- it can
   * involve DB lookups -- so we need to think about how to restructure things accordingly.
   */
  def withSpace(
        requireLogin:Boolean, 
        ownerIdStr:String,
        spaceId:String, 
        thingIdStr:Option[String] = None,
        errorHandler:Option[PartialFunction[(ThingFailed, RequestContext), Result]] = None
      )(f: (RequestContext => Result)):EssentialAction = withUser(false) { rc =>
    val requester = rc.requester getOrElse User.Anonymous
    val thingId = thingIdStr map (ThingId(_))
    val ownerId = getIdentityByThingId(ownerIdStr)
    def withFilledRC(rc:RequestContext, stateOpt:Option[SpaceState], thingOpt:Option[Thing])(cb:RequestContext => Result):Result = {
      val filledRC = rc.copy(ownerId = ownerId, state = stateOpt, thing = thingOpt)
      // Give the listeners a chance to chime in:
      val updatedRC = PageEventManager.requestReceived(filledRC)
      val state = stateOpt.get
      val result =
        // Okay, now we have enough information to check whether we have a Space-specific authorization:
        if (updatedRC.redirectTo.isDefined)
          Redirect(updatedRC.redirectTo.get)
        else if ((requireLogin && updatedRC.requester.isEmpty) || !state.canRead(updatedRC.requester.getOrElse(User.Anonymous), thingOpt.map(_.id).getOrElse(state)))
          onUnauthorized(updatedRC.request)
        else
          cb(updatedRC)
      updatedRC.updateSession(result)
    }
    askSpaceMgr[ThingResponse](GetThing(requester, ownerId, ThingId(spaceId), thingId)) {
      case ThingFound(id, state) => {
        val thingOpt = id match {
          case UnknownOID => None
          case oid:OID => state.anything(oid)
        }
        // Log what we got back -- turn this on as needed:
        //QLog.spewThing(thingOpt.getOrElse(state))
        if (thingIdStr.isDefined && thingOpt.isEmpty)
          doError(routes.Application.index, "That wasn't a valid path")
        else {
          withFilledRC(rc, Some(state), thingOpt)(f)
        }
      }
      case err:ThingFailed => {
        val ThingFailed(error, msg, stateOpt) = err
        if (stateOpt.isDefined)
          withFilledRC(rc, stateOpt, None)(filledRC => errorHandler.flatMap(_.lift((err, filledRC))).getOrElse(doError(routes.Application.index, msg)))
        else
          onUnauthorized(rc.request)
      }
    }     
  }

  /**
   * Convenience wrapper for withSpace -- use this for pages that are talking about
   * a specific Thing.
   */
  def withThing(requireLogin:Boolean, ownerId:String, spaceId:String, thingIdStr:String,
        errorHandler:Option[PartialFunction[(ThingFailed, RequestContext), Result]] = None) = { 
    withSpace(requireLogin, ownerId, spaceId, Some(thingIdStr), errorHandler) _
  }
}