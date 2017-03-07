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

import UserLevel._

case class SignupInfo(email:String, password:String, handle:String, display:String)
  
/**
 * TODO: we need to tease a more abstract interface out of here, which is all that should be visible to
 * most subsystems. Nobody outside the Identity system itself should be able to easily access the
 * mapping from Users to Identities. I suspect that teasing this apart will reveal some leaks.
 */
trait User {
  def id:OID
  // TODO: this should be Option[String]!
  def name:String
  // TODO: this doesn't belong in the base User trait -- it leaks Identity info!
  def identities:Seq[Identity]
  def level:UserLevel
  def tosVersion:Int
  
  def levelName:String = UserLevel.levelName(level)
  
  def isActualUser = UserLevel.isActualUser(level)
  
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
  
  def fullIdentityById(identityId:IdentityId) = {
    for {
      identity <- identityById(identityId)
    }
      yield FullIdentity(identity.id, identity.email, identity.handle, identity.name, id, identity.kind)
  }
  
  // TODO: this is a bit crude so far, and doesn't cope with the notion that I might have
  // multiple logins. But it's a useful shortcut to start.
  // WARNING: should only be used by Admin!!!
  def loginIdentity = identityBy(_.kind == IdentityKind.QuerkiLogin)
  // TODO: this is a rough and ready concept of "give me the most interesting identity for this User".
  // WARNING: should only be used by Admin!!!
  def mainIdentity = loginIdentity getOrElse identityBy(_ => true).get
  
  /**
   * This defines whether this User is allowed to own Spaces or not. Originally, PendingUsers weren't
   * allowed to own Spaces, but it turns out to be necessary to loosen this in order to make the
   * signup workflow not suck. (Especially for instantiating Apps: we want to be able to create the
   * Space *before* responding to the validation email.)
   */
  def canOwnSpaces = level >= PendingUser
  
  def isAdmin = UserLevel.isAdmin(level)
  
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
  
  // Equality is based on the user ID. Recipe from http://www.artima.com/pins1ed/object-equality.html#28.4
  def canEqual(other:Any):Boolean = other.isInstanceOf[User]
  override def equals(other:Any):Boolean = {
    other match {
      case that:User => (that canEqual this) && (that.id == id)
      case _ => false
    }
  }
  override def hashCode:Int = 41 * (41 + id.hashCode)
}

case class FullUser(id:OID, name:String, identities:Seq[Identity] = Seq.empty, level:UserLevel = UnknownUserLevel, tosVersion:Int = 0) extends User

object User {
  val userIdSessionParam = "userId"
  val levelSessionParam = "lvl"
  val guestIdSessionParam = "guestId"
  val guestEmailSessionParam = "guestEmail"
    
  case object Anonymous extends User {
    val id = UnknownOID
    val name = ""
    val identities = Seq(Identity.AnonymousIdentity)
    val level = UnknownUserLevel
    val tosVersion = noTOSUserVersion
  }
}

/**
 * Pseudo-User for SimpleEmail Identities.
 */
case class GuestUser(identity:Identity) extends User {
  // We reuse the IdentityId as the UserId, so that the different Guests get different Sessions:
  def id = identity.id
  val name = ""
  val identities = Seq(identity)
  val level = SpaceSpecific
  val tosVersion = noTOSUserVersion
  
  override def toSession:Seq[(String, String)] = {
    if (identity.email.addr.length == 0) {
      Seq(
        (User.guestIdSessionParam -> identity.id.toString)
      )
    } else {
      Seq(
        (User.guestIdSessionParam -> identity.id.toString),
        (User.guestEmailSessionParam -> identity.email.addr)
      )
    }
  }
}

// TODO: these Kinds really ought to be represented by different classes! Note that Trivial
// is even simpler than PublicIdentity -- it is an id and *nothing* else. So we ought to
// have Identity as a base trait, with subtraits and base classes under it. The current
// approach is kind of horribly fragile, with the "significant" fields defined more by
// convention than anything rigorous.
object IdentityKind {
  val SimpleEmail = 1
  val QuerkiLogin = 2
  val Anonymous = 3
  val Trivial = 4
  
  type IdentityKind = Int
}
import IdentityKind._

// TODO: In all of the below, handle should be Option[String]!!!!!

/**
 * This represents the information that others can see about someone.
 */
trait PublicIdentity {
  def id:OID
  def handle:String
  def name:String
}

case class SimpleIdentity(id:OID, handle:String, name:String) extends PublicIdentity

case class Identity(id:OID, email:EmailAddress, auth:String, handle:String, name:String, kind:IdentityKind) extends PublicIdentity

case class FullIdentity(id:OID, email:EmailAddress, handle:String, name:String, userId:UserId, kind:IdentityKind) extends PublicIdentity

object Identity {
  val AnonymousOID = OID(-2)
  object AnonymousIdentity 
    extends Identity(AnonymousOID, EmailAddress("anonidentity@querki.net"), "", "Anonymous", "Anonymous", IdentityKind.Anonymous) with Serializable
}
