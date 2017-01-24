package querki.session

import scala.concurrent.Future

import models.Wikitext
import querki.data.{SpaceInfo, TID, UserInfo}
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
   * Creates a new Space with the given name, and returns its information. If an App is
   * specified, this Space will be created as an instance of that App.
   */
  def createSpace(name:String, appId:Option[TID]):Future[SpaceInfo]
  
  /**
   * Should only be called for new Users. Sends a fresh activation email for their account.
   */
  def resendActivationEmail():Future[Unit]
  
  /**
   * Given the hash from an activation link, this confirms that it is valid, and moves
   * the account to full status if so.
   */
  def validateActivationHash(validationStr:String):Future[Boolean]
  
  /**
   * Changes the complexity level preferred by this user.
   */
  def setComplexity(level:TID):Future[TID]
  
  /**
   * Checks whether this User's TOS agreement is up to date.
   */
  def checkTOS():Future[TOSState]
  
  /**
   * Fetch the current Terms of Service.
   */
  def fetchTOS():Future[TOSInfo]
  
  /**
   * Officially records that the logged-in User has agreed to the specified version of the
   * TOS.
   */
  def agreeToTOS(version:Int):Future[Unit]
}

object UserFunctions {
  case class AllSpaces(mySpaces:Seq[SpaceInfo], memberOf:Seq[SpaceInfo])
  case class AccountInfo(handle:String, displayName:String, email:String, level:UserLevel.UserLevel)
  
  /**
   * Describes the current Terms of Service.
   */
  case class TOSInfo(version:Int, text:Wikitext)
  
  sealed trait TOSState
  case object TOSOkay extends TOSState
  case object TOSOld extends TOSState
}
