package querki.identity

import scala.util._

import anorm._
import play.api._
import play.api.db._
import play.api.mvc._
import play.api.Play.current

import models.{OID,UnknownOID}
import models.{ThingId,AsOID,AsName}

import querki.core.NameUtils
import querki.db.ShardKind
import ShardKind._

import querki.system.TOSModule.noTOSUserVersion

import querki.util._
import SqlHelpers._

import querki.email.EmailAddress

import MOIDs._

object UserLevel {
  type UserLevel = Int
  
  val UnknownUserLevel = 0
  val PendingUser = 1
  val FreeUser = 2
  val PaidUser = 3
  val PermanentUser = 4
  
  val SpaceSpecific = 5
  
  val AdminUser = 10
  
  val SuperadminUser = 100
  
  def levelName(level:UserLevel) = level match {
    case PendingUser => "invited"
    case FreeUser => "free"
    case PaidUser => "paid"
    case PermanentUser => "permanent"
    case AdminUser => "admin"
    case SuperadminUser => "superadmin"
      
    case _ => "Unknown: " + level.toString
  }
}

import UserLevel._

case class SignupInfo(email:String, password:String, handle:String, display:String)
  
trait User {
  def id:OID
  // TODO: this should be Option[String]!
  def name:String
  def identities:Seq[Identity]
  def level:UserLevel
  def tosVersion:Int
  
  def levelName:String = UserLevel.levelName(level)
  
  // TODO: this probably needs to be rethought. It is what we current use for Security.username,
  // but won't exist for non-login accounts. So we need to be able to do other things for those.
  // Possibly we should be putting the OID in there instead.
  private def handle:String = {
    val mainIdentity = identityBy(_.kind == IdentityKind.QuerkiLogin)
    mainIdentity.map(_.handle).getOrElse("")
  }
  
  def hasIdentity(identity:OID):Boolean = identities.exists(_.id == identity)
   
  def toSession:Seq[(String, String)] = {
    Seq(
      (Security.username -> handle),
      (User.userIdSessionParam -> id.toString),
      (User.levelSessionParam -> level.toString)
    )
  }
  
  // TODO: we'll need to cope with users who don't have a name, since that's a
  // paid-user feature. In that case, return a ThingId'ized id.
  def toThingId = AsName(name)
  
  def identityBy(pred:Identity => Boolean) = identities.find(pred)
  
  def identityByHandle(handle:String) = identityBy(_.handle.equalsIgnoreCase(handle))
  def identityById(identityId:OID) = identityBy(_.id == identityId)
  
  // TODO: this is a bit crude so far, and doesn't cope with the notion that I might have
  // multiple logins. But it's a useful shortcut to start.
  // WARNING: should only be used by Admin!!!
  def loginIdentity = identityBy(_.kind == IdentityKind.QuerkiLogin)
  // TODO: this is a rough and ready concept of "give me the most interesting identity for this User".
  // WARNING: should only be used by Admin!!!
  def mainIdentity = loginIdentity getOrElse identityBy(_ => true).get
  
  /**
   * This defines whether this User is allowed to own Spaces or not. Basically, Pending Users aren't
   * allowed to create or receive Spaces.
   */
  def canOwnSpaces = level >= FreeUser
  
  def isAdmin = (level == AdminUser || level == SuperadminUser)
  
  /**
   * If you have a function that is high-security, and should not be accessible by non-Admins, wrap the guts
   * of the function in this. It is effectively a runtime assertion, but the expense is worth it for a bit
   * of security protection against logic bugs.
   */
  def requireAdmin[T](f: => T):T = {
    if (isAdmin)
      f
    else
      throw new InternalException("Illegal attempt to call a function that requires admin rights. Caller is " + toThingId)
  }
}

case class FullUser(id:OID, name:String, identities:Seq[Identity] = Seq.empty, level:UserLevel = UnknownUserLevel, tosVersion:Int = 0) extends User

// Internal System User. This should be used for making internal changes to Spaces that are *not* under the
// aegis of the requesting user. 
//
// USE WITH EXTREME CAUTION! Don't mess with this if you don't understand it! The SystemUser has essentially
// unlimited rights, so should only be invoked when we are intentionally doing something on the user's
// behalf that they cannot do themselves. That automatically requires a security audit.
case object SystemUser extends User {
  val id = SystemUserOID
  val name = "SystemUser"
  lazy val email = EmailAddress(Config.getString("querki.mail.systemFrom", "querki@querki.net"))
  // TODO: the presence of a reference to models.system here is suspicious. Does this indicate that SystemUser
  // doesn't belong in this file? Likely so.
  val identities = Seq(Identity(SystemIdentityOID, email, "", "systemUser", name, IdentityKind.QuerkiLogin))
  val level = SuperadminUser
  val tosVersion = noTOSUserVersion
}

object User {
  val userIdSessionParam = "userId"
  val levelSessionParam = "lvl"
    
  object Anonymous extends User {
    val id = UnknownOID
    val name = ""
    val identities = Seq.empty
    val level = UnknownUserLevel
    val tosVersion = noTOSUserVersion
  }
}

object IdentityKind {
  val SimpleEmail = 1
  val QuerkiLogin = 2
  
  type IdentityKind = Int
}
import IdentityKind._

case class Identity(id:OID, email:EmailAddress, auth:String, handle:String, name:String, kind:IdentityKind)
