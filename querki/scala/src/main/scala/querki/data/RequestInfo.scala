package querki.data

/**
 * The client-side version of the server's RequestContext. Contains all information from the RC that
 * is needed client-side.
 */
case class RequestInfo(
  user:Option[UserInfo], 
  space:Option[ThingInfo],
  thing:Option[ThingInfo],
  ownerHandle:String,
  isOwner:Boolean)