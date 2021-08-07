package controllers

import play.api.mvc._
import models._

import language.implicitConversions
import querki.ecology._
import querki.identity.{IdentityId, User}

import querki.ui.UIRenderer
import querki.values.{RequestContext, SpaceState}

trait RequestHeaderParser {
  val request: RequestHeader
  val sessionUpdates: Seq[(String, String)]
  val returnToHere: Boolean

  def hasQueryParam(paramName: String) = request.queryString.contains(paramName)

  def queryParam(paramName: String): Seq[String] =
    if (hasQueryParam(paramName)) request.queryString(paramName) else Seq.empty

  def firstQueryParam(paramName: String): Option[String] = {
    val seq = queryParam(paramName)
    if (seq.isEmpty) None else Some(seq.head)
  }

  def paramIs(
    paramName: String,
    value: String
  ) = hasQueryParam(paramName) && firstQueryParam(paramName).map(_ == value).getOrElse(false)
  def isTrue(paramName: String) = paramIs(paramName, "true")

  // Mechanism for returning to this request after a redirect. Add this to the sessionUpdates if you
  // will want to come back here.
  val returnToParam = "returnTo"
  def returnToHereUpdate = Map((returnToParam -> request.path))

  def updateSession(result: Result): Result = {
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
      val newSession = (request.session /: updates)((sess, update) => sess + (update._1 -> update._2))
      result.withSession(newSession)
    }
  }
}

case class SimpleRequestHeaderParser(
  request: RequestHeader,
  sessionUpdates: Seq[(String, String)],
  returnToHere: Boolean
) extends RequestHeaderParser

/**
 * The Play-specific version of a RequestContext.
 *
 * Everything that knows about HTTP specifically should go in here. This deliberately belongs up in controllers,
 * at the Play level, and should not generally be exposed anywhere else. (There is some old cruft pointing to it
 * from the lower levels -- those should be considered refactoring targets.)
 */
case class PlayRequestContextFull[B](
  request: Request[B],
  requester: Option[User],
  ownerId: IdentityId,
  error: Option[String] = None,
  sessionUpdates: Seq[(String, String)] = Seq.empty,
  redirectTo: Option[Call] = None,
  spaceIdOpt: Option[String] = None,
  reqOwnerHandle: Option[String] = None
) extends RequestHeaderParser {
  lazy val rc = RequestContext(requester, ownerId, spaceIdOpt)
  def requesterOrAnon = rc.requesterOrAnon

  // NOTE: this may be wrong, but at the moment is the way the logic works
  val returnToHere: Boolean = false

  def sessionCookie(name: String) = request.session.get(name)

  def returningToHere = copy(sessionUpdates = sessionUpdates ++ returnToHereUpdate)

  def withError(err: String) = copy(error = Some(err))

  /**
   * This looks for a previously-set returnToHere. If found, it redirects there; otherwise, it redirects to the
   * specified other Call.
   */
  def returnToPreviousOr(other: Call): Result = {
    val redirectOpt = sessionCookie(returnToParam)
    redirectOpt match {
      case Some(redirect) => {
        val session = request.session - returnToParam
        Results.Redirect(redirect).withSession(session)
      }
      case None => Results.Redirect(other)
    }
  }
}
