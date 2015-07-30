package querki.session

import scala.concurrent.Future

import querki.data.SpaceInfo
import querki.identity.UserLevel

/**
 * Represents non-Space functions that any logged-in user can perform. Mainly
 * relates to this user's own information.
 * 
 * @author jducoeur
 */
trait UserFunctions {
  import UserFunctions._
  
  /**
   * Fetch the Spaces that I am involved with.
   */
  def listSpaces():Future[AllSpaces]
  
  /**
   * Fetch all of my account information, for the Account Page.
   */
  def accountInfo():Future[AccountInfo]
}

object UserFunctions {
  case class AllSpaces(mySpaces:Seq[SpaceInfo], memberOf:Seq[SpaceInfo])
  case class AccountInfo(handle:String, displayName:String, email:String, level:UserLevel.UserLevel)
}
