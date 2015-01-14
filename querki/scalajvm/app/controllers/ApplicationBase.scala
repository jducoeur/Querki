package controllers

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits._

import play.api.Logger
import play.api.mvc._

import models._

import querki.ecology._
import querki.identity._
import querki.session.messages.GetThing
import querki.spaces.SpaceManager
import querki.spaces.messages._
import querki.util._
import querki.values.SpaceState

class ApplicationBase extends Controller with EcologyMember {
  
  implicit var ecology:Ecology = null
  
  lazy val AccessControl = interface[querki.security.AccessControl]
  lazy val IdentityAccess = interface[querki.identity.IdentityAccess]
  lazy val UserAccess = interface[querki.identity.UserAccess]
  lazy val PageEventManager = interface[controllers.PageEventManager]
  lazy val UserSessionMgr = interface[querki.session.Session]
  lazy val SpaceOps = interface[querki.spaces.SpaceOps]
  
  def fRes(res:Result) = Future.successful(res)
  
  // TBD: this is kind of horrifyingly over-clever, but useful given how variable we are in having
  // immediate vs. future results. Is it a decent answer?
  implicit def result2Future(res:Result):Future[Result] = Future.successful(res)
    
  /**
   * Standard error handler. Iff you get an error and the correct response is to redirect to
   * another page, use this. The only exception is iff you need to preserve data, and thus want
   * to simply redisplay the current page; in that case, set the error in the RequestContext before
   * constructing the page.
   */
  def doError(redirectTo:Call, errorMsg:String):Future[Result] = {
    // TODO: figure out a better way to do this, and make it configurable:
    try {
      throw new Exception("Got error; redirecting: " + errorMsg)
    } catch {
      case e:Throwable => Logger.info(e.toString, e)
    }
    Redirect(redirectTo).flashing("error" -> errorMsg)
  }
  
  def doError(redirectTo:Call, ex:PublicException)(implicit rc:PlayRequestContext):Future[Result] = doError(redirectTo, ex.display(rc.request))
  
  def unknownSpace(spaceId:String):Result = 
    Redirect(routes.Application.index).flashing("error" -> s"Either $spaceId doesn't exist, or you don't have permission to read it.")
  
  def doInfo(redirectTo:Call, msg:String):Result = {
    Redirect(redirectTo).flashing("info" -> msg)
  }
  
  def getOwnerIdentity(thingIdStr:String):Future[OID] = {
    val thingId = ThingId(thingIdStr)
    thingId match {
      case AsOID(oid) => Future.successful(oid)
      case AsName(handle) => {
        if (handle.length() == 0) Future.successful(UnknownOID)
        else {
          // getIdentity() returns Future[Option[PublicIdentity]]
          IdentityAccess.getIdentity(handle).map(_.map(_.id).getOrElse(UnknownOID))
        }
      }
    }
  }
  
  def userFromSession(request:RequestHeader):Future[Option[User]] = IdentityAccess.userFromSession(request)
  // Workaround to deal with the fact that Security.Authenticated has to get a non-empty
  // result in order to let things through. So if a registered user is *optional*, we need to
  // return something:
  def forceUser(request: RequestHeader):Option[Future[User]] = Some(userFromSession(request).map(_ getOrElse User.Anonymous))

  def onUnauthorized(request: RequestHeader) = {
    // Send them over to the login page, but record that we want to return to this page
    // once they do log in:
    val rc = SimpleRequestHeaderParser(request, Seq.empty, true)
    rc.updateSession(Redirect(routes.Application.index))
  }

  // Fetch the User from the session, or User.Anonymous if they're not found.
  def withAuth(f: => User => Request[AnyContent] => Future[Result]):EssentialAction = {
    withAuth(BodyParsers.parse.anyContent)(f)
  }
  def withAuth[B](parser:BodyParser[B])(f: => User => Request[B] => Future[Result]):EssentialAction = {
    val handler = { userFut:Future[User] =>
      Action.async(parser) { request =>
        // When we've resolved who is asking, then keep going...
        userFut flatMap { user =>
          f(user)(request) 
        }
      }
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
  def withUser[B](requireLogin:Boolean, parser:BodyParser[B] = BodyParsers.parse.anyContent)(f: PlayRequestContext => Future[Result]) = withAuth(parser) { user => implicit request =>
    if (requireLogin && user == User.Anonymous) {
      Future.successful(onUnauthorized(request))
    } else {
      // Iff requireLogin was false, we might not have a real user here, so massage it:
      val userParam = if (user == User.Anonymous) None else Some(user)
      userParam match {
        case Some(u) => {
          UserSessionMgr.getSessionInfo(user) flatMap { info =>
            f(PlayRequestContextFull(request, userParam, UnknownOID, None, None, ecology, numNotifications = info.numNewNotes))          
          }
        }
        case None => f(PlayRequestContextFull(request, userParam, UnknownOID, None, None, ecology))
      }
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
  def askSpaceMgr[A](msg:SpaceMgrMsg)(cb: A => Future[Result])(implicit m:Manifest[A]) = {
    SpaceOps.askSpaceManager[A, Result](msg)(cb)
  }
  
  /**
   * Newer and simpler version of askSpaceMgr. Note that the callback can and should do something
   * appropriate with resulting errors!
   * 
   * TODO: we should generalize the error handling here.
   */
  def askSpace(msg:SpaceMgrMsg)(cb: PartialFunction[Any, Future[Result]]):Future[Result] = {
    SpaceOps.askSpaceManager2(msg)(cb)
  }
  
  /**
   * Fetch enough routing information to be able to send messages through the SpaceManager.
   * Note that this is the usual replacement for withSpace -- it fetches just enough info to
   * send messages off to the UserSession level, and nothing more.
   */
  def withRouting
    (ownerIdStr:String, spaceId:String)
    (f: (PlayRequestContext => Future[Result])):EssentialAction = 
  withUser(false) { originalRC =>
    for {
      ownerId <- getOwnerIdentity(ownerIdStr)
      rc = originalRC.copy(ownerId = ownerId, spaceIdOpt = Some(spaceId), reqOwnerHandle = Some(ownerIdStr))
      result <- f(rc)
    }
      yield result
  }
  
  /**
   * Given a Space, this fetches that Space's current state before calling the core logic.
   * 
   * TBD: Why the ridiculous return signature? Because I am getting cryptic errors about withSpace
   * being recursive otherwise.
   * 
   * TODO: this is fundamentally broken at the moment. withUser() is potentially long-running -- it can
   * involve DB lookups -- so we need to think about how to restructure things accordingly.
   * 
   * TODO: all uses of withSpace should be considered bad smells, and deprecated.
   */
//  @deprecated("withSpace is fundamentally broken -- it requires fetching the SpaceState to the Play level. Gradually replace it.", "0.11.0")
  def withSpace[B](
        requireLogin:Boolean, 
        ownerIdStr:String,
        spaceId:String, 
        thingIdStr:Option[String] = None,
        errorHandler:Option[PartialFunction[(ThingResponse, PlayRequestContext), Result]] = None,
        // This is a pretty rare parameter. It should only be used if we want to execute this function
        // even if the requester does not have read access to this Space. (As during invitation handling.)
        allowAnyone:Boolean = false,
        parser:BodyParser[B] = BodyParsers.parse.anyContent
      )(f: (PlayRequestContext => Future[Result])):EssentialAction = withUser(false, parser) { originalRC =>
    val requester = originalRC.requester getOrElse User.Anonymous
    val thingId = thingIdStr map (ThingId(_))
    // Give the listeners a chance to chime in. Note that this is where things like invitation
    // management come into play. This needs to happen *BEFORE* we try to fetch the Space, because
    // an invitation might be to someone who isn't allowed to read the Space. Things should
    // hook requestReceived iff they just need the raw request, and don't need to be able to
    // read the Space.
    val rcWithPath = originalRC.copy(spaceIdOpt = Some(spaceId), reqOwnerHandle = Some(ownerIdStr))
    val updatedRC = PageEventManager.requestReceived(rcWithPath)
    if (updatedRC.redirectTo.isDefined) {
      Future.successful(updatedRC.updateSession(Redirect(updatedRC.redirectTo.get)))      
    } else {
	    def withFilledRC(rc:PlayRequestContext, ownerId:OID, stateOpt:Option[SpaceState], thingOpt:Option[Thing])(cb:PlayRequestContext => Future[Result]):Future[Result] = {
	      val filledRC = rc.copy(ownerId = ownerId, state = stateOpt, thing = thingOpt)
	      val state = stateOpt.get
	      val result:Future[Result] =
	        if ((requireLogin && filledRC.requester.isEmpty) || 
	            (!allowAnyone && !AccessControl.canRead(state, filledRC.requester.getOrElse(User.Anonymous), thingOpt.map(_.id).getOrElse(state))))
	          onUnauthorized(filledRC.request)
	        else
	          cb(filledRC)
	      result
	    }
	    getOwnerIdentity(ownerIdStr).flatMap { ownerId =>
		    askSpaceMgr[ThingResponse](SessionRequest(requester, ownerId, ThingId(spaceId), GetThing(thingId))) {
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
		          withFilledRC(updatedRC, ownerId, Some(state), thingOpt)(f)
		        }
		      }
		      case err @ ThingError(error, stateOpt) => {
		        if (stateOpt.isDefined)
		          withFilledRC(updatedRC, ownerId, stateOpt, None) { implicit filledRC =>
		            errorHandler.flatMap(_.lift((err, filledRC))).map(fRes(_)).getOrElse(doError(routes.Application.index, error))
		          }
		        else
		          onUnauthorized(updatedRC.request)	        
		      }
		    }     
	    }
    }
  }

  /**
   * Convenience wrapper for withSpace -- use this for pages that are talking about
   * a specific Thing.
   */
  def withThing[B](requireLogin:Boolean, ownerId:String, spaceId:String, thingIdStr:String,
        errorHandler:Option[PartialFunction[(ThingResponse, PlayRequestContext), Result]] = None, 
        parser:BodyParser[B] = BodyParsers.parse.anyContent) = 
  { 
    withSpace(requireLogin, ownerId, spaceId, Some(thingIdStr), errorHandler, parser = parser) _
  }
}