package querki.data

import querki.identity.UserLevel.UserLevel

/**
 * The client-side version of the server's RequestContext. Contains all information from the RC that
 * is needed client-side.
 */
case class RequestInfo(
  user: Option[UserInfo],
  space: Option[SpaceInfo],
  isOwner: Boolean,
  userLevel: UserLevel,
  // If specified, the PageManager will go to this page immediately after load:
  navigateToOpt: Option[String] = None,
  // An opaque payload, intended to go with navigateToOpt:
  payloadOpt: Option[String] = None,
  forbidden: Boolean = false
)
