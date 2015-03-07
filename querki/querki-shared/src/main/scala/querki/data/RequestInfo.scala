package querki.data

import querki.pages.PageDetails

/**
 * The client-side version of the server's RequestContext. Contains all information from the RC that
 * is needed client-side.
 */
case class RequestInfo(
  user:Option[UserInfo], 
  space:Option[SpaceInfo],
  isOwner:Boolean,
  isAdmin:Boolean,
  forbidden:Boolean = false
  )
