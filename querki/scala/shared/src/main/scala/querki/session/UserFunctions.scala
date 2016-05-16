package querki.session

import scala.concurrent.Future

import querki.data.{SpaceInfo, UserInfo}
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
  
  /**
   * Changes my password. Throws an exception if anything goes wrong. (But assumes that
   * the UI is doing sensible sanity-checking first.)
   */
  def changePassword(oldPassword:String, newPassword:String):Future[Unit]
  
  /**
   * Changes my display name, and returns an updated record for me.
   */
  def changeDisplayName(newDisplayName:String):Future[UserInfo]
  
  /**
   * Creates a new Space with the given name, and returns its information.
   */
  def createSpace(name:String):Future[SpaceInfo]
  
  /**
   * Should only be called for new Users. Sends a fresh activation email for their account.
   */
  def resendActivationEmail():Future[Unit]
  
  /**
   * Given the hash from an activation link, this confirms that it is valid, and moves
   * the account to full status if so.
   */
  def validateActivationHash(validationStr:String):Future[Boolean]
}

object UserFunctions {
  case class AllSpaces(mySpaces:Seq[SpaceInfo], memberOf:Seq[SpaceInfo])
  case class AccountInfo(handle:String, displayName:String, email:String, level:UserLevel.UserLevel)
}
