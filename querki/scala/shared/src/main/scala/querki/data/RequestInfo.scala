package querki.data

import querki.identity.UserLevel.UserLevel

/**
 * The client-side version of the server's RequestContext. Contains all information from the RC that
 * is needed client-side.
 */
case class RequestInfo(
  user:Option[UserInfo], 
  space:Option[SpaceInfo],
  isOwner:Boolean,
  userLevel:UserLevel,
  forbidden:Boolean = false
  )
