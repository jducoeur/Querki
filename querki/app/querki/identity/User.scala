package querki.identity

import scala.util._

import anorm._
import play.api._
import play.api.db._
import play.api.mvc._
import play.api.Play.current

import models.{OID,UnknownOID}
import models.{ThingId,AsOID,AsName}
import models.system

import querki.db.ShardKind
import ShardKind._

import querki.util._
import SqlHelpers._

import modules.email.EmailAddress

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
  val id = models.system.OIDs.SystemUserOID
  val name = "SystemUser"
  lazy val email = EmailAddress(Config.getString("querki.mail.systemFrom", "querki@querki.net"))
  // TODO: the presence of a reference to models.system here is suspicious. Does this indicate that SystemUser
  // doesn't belong in this file? Likely so.
  val identities = Seq(Identity(models.system.OIDs.SystemIdentityOID, email, "", "systemUser", name, IdentityKind.QuerkiLogin))
  val level = SuperadminUser
  val tosVersion = -1
}

// TODO: given that this is full of methods that shouldn't be synchronous, this should
// probably be implemented behind some worker actors instead.
object User {
  val userIdSessionParam = "userId"
  val levelSessionParam = "lvl"
    
  object Anonymous extends User {
    val id = UnknownOID
    val name = ""
    val identities = Seq.empty
    val level = UnknownUserLevel
    val tosVersion = -1
  }
  
  /**
   * We look users up by name pretty frequently, so I'm going to tentatively put a cache here,
   * just to reduce DB traffic.
   * 
   * TODO: this really ought to age out. I don't expect entries to be removed often -- indeed, I
   * expect users to release names quite rarely -- but we should be able to cope with it.
   * 
   * TODO: this isn't sufficiently thread-safe! This is probably a good place to use an Agent,
   * as soon as we upgrade to the current version of Akka.
   */
  private val idByNameCache = collection.mutable.Map.empty[String, OID]
  
  /**
   * Same thing, other direction.
   * 
   * TODO: same as idByNameCache
   * 
   * DEPRECATED: this is now obviated by the userByIdCache.
   */
  private val nameByIdCache = collection.mutable.Map.empty[OID, String]
  
  /**
   * TODO: RE-ADD CACHING
   * 
   * Note that caching needs to fundamentally change, though. We should really have a single master
   * cache of *all* of the looked-up Users, which is probably controlled by getUser(). But that gets
   * indexed by a bunch of little caches of pointers into it by various keys, which correspond to the
   * query being passed into getUser(). Odds are that the cache is itself essentially a parameter to
   * getUser().
   * 
   * This is going to be a hassle, so don't be half-assed about it: better to do it right once, than
   * spend a bunch of effort on an optimization we don't need yet. Make sure it winds up thread-safe,
   * which is not a small issue, and deal with making all of this properly asynchronous while we're at it.
   * 
   * TBD: is this the right way to do it? Or should we instead be caching at a higher semantic level?
   * That might actually make more sense. Do some profiling of how we are using these records, to see.
   */
  
  private def rowToUser(row:SqlRow):User = {
    val email = EmailAddress(row.string("email"))
    val identityOID = row.oid("id")
    FullUser(row.oid("userId"), row.string("name"),
      Seq(Identity(identityOID, email, row.string("authentication"), row.string("handle"), row.string("name"), IdentityKind.QuerkiLogin)),
      row.int("level"),
      row.int("tosVersion"))
  }
  
  private def getUser(query:Sql, checkOpt:Option[User => Boolean] = None):Option[User] = {
    // TODO: check the cache? Or should that be done at the semantic levels below?
    // There's a case to be made that the cache check is necessarily tied to the query,
    // and therefore just plain doesn't belong here...
    DB.withConnection(dbName(System)) { implicit conn =>
      val result = query()
      result.headOption.flatMap { row =>
//        val email = EmailAddress(row.string("email"))
//        val identityOID = row.oid("id")
//        // Create the User record
        val user = rowToUser(row)
//          FullUser(row.oid("userId"), row.string("name"),
//          Seq(Identity(identityOID, email, row.string("authentication"), row.string("handle"), row.string("name"), IdentityKind.QuerkiLogin)),
//          row.int("level"))
        
        // If this is conditional -- for example, if this is login, and we need to authenticate --
        // then do the check before returning the found user
        if (checkOpt.isDefined)
          checkOpt.flatMap { fCheck =>
            if (fCheck(user))
              Some(user)
            else
              None
          }
        else
          Some(user)
      }
    }    
  }
  
  def userLoadSqlWhere(whereClause:String) = SQL("""
        SELECT Identity.id, Identity.name, userId, User.level, authentication, email, handle, User.tosVersion FROM Identity
          JOIN User ON User.id=userId
        WHERE """ + whereClause + ";")
  
  private def loadByEmail(email:EmailAddress, checkOpt:Option[User => Boolean]):Option[User] = {
    val identityQuery = userLoadSqlWhere("""email={email}""").on("email" -> email.addr.toLowerCase())
    getUser(identityQuery, checkOpt)
  }
  
  private def loadByHandle(rawHandle:String, checkOpt:Option[User => Boolean]):Option[User] = {
    val handle = system.NameType.canonicalize(rawHandle)
    val personQuery = userLoadSqlWhere("""handle={handle} and kind={loginKind}""").on("handle" -> handle, "loginKind" -> IdentityKind.QuerkiLogin)
    getUser(personQuery, checkOpt)
  }
  
  /**
   * WARNING: this should only be called in the context of an admin call!
   */
  def getAllForAdmin(requester:User):Seq[User] = requester.requireAdmin {
    val userQuery = userLoadSqlWhere("""User.level != 0""")
    DB.withConnection(dbName(System)) { implicit conn =>
      val result = userQuery()
      result.map(rowToUser(_)).force
    }
  }
  
  /**
   * If we find the username in the current session, return a populated Some(User); otherwise, None.
   * 
   * TODO: cache the full record in the cookie! Note that this is closely related to User.toSession().
   */
  def get(request:RequestHeader) = {
    val username = request.session.get(Security.username)
    username.flatMap(loadByHandle(_, None))
  }
  
  /**
   * Global method to fetch an Identity (and a bit of User info) from a ThingId.
   */
  def getIdentity(thingId:ThingId):Option[(Identity, UserLevel)] = {
    thingId match {
      case AsOID(id) => { 
        for (
          user <- getUserForIdentity(id);
          identity <- user.identityById(id)
            )
          yield (identity, user.level)
      }
      case AsName(handle) => {
        for (
          user <- loadByHandle(handle, None);
          identity <- user.identityByHandle(handle)
            )
          yield (identity, user.level)
      }
    }
  }
 
  // TODO: this shouldn't be synchronous! There's a DB call in it, so it should be async.
  def getIdentity(rawHandle:String) = {
    loadByHandle(rawHandle, None).flatMap(_.identityByHandle(rawHandle)).map(_.id)
  }
  
  private def getUserForIdentity(id:OID):Option[User] = {
    val query = userLoadSqlWhere("Identity.id={id}").on("id" -> id.raw)
    getUser(query)
  }
  
  def getIdentity(id:OID):Option[Identity] = getUserForIdentity(id).flatMap(_.identityById(id))
  
  // TODO: this shouldn't be synchronous! There's a DB call in it, so it should be async.
  private def checkQuerkiLoginByEmail(email:EmailAddress, passwordEntered:String):Option[User] = {
    loadByEmail(
      email, 
      Some({ user:User => 
        val identityOpt = user.identityBy(_.email.addr == email.addr)
        identityOpt.map(identity => Hasher.authenticate(passwordEntered, EncryptedHash(identity.auth))).getOrElse(false) 
      })
    )
  }
  
  // TODO: this shouldn't be synchronous! There's a DB call in it, so it should be async.
  def checkQuerkiLogin(login:String, passwordEntered:String):Option[User] = {
    if (login.contains("@")) {
      checkQuerkiLoginByEmail(EmailAddress(login), passwordEntered)
    } else {
      loadByHandle(
        login, 
        Some({ user:User => 
          val identityOpt = user.identityByHandle(login)
          identityOpt.map(identity => Hasher.authenticate(passwordEntered, EncryptedHash(identity.auth))).getOrElse(false) 
        })
      )
    }
  }
  
  // TODO: this is a pretty naive use of Try, but we should get into the habit. We likely should develop a
  // higher-level framework on top of Try, that allows us to pass more-structured errors up the line. Note that
  // some of these exceptions are routine user-input problems like a duplicate email address, while others are
  // serious internal exceptions. Figure out how we want to handle them differently.
  def createProvisional(info:SignupInfo):Try[User] = Try {
    // Note that both of these will either return None or throw an exception:
    val existingOpt = loadByHandle(info.handle, 
        Some({_ => throw new PublicException("User.handleExists", info.handle)}))
    val emailOpt = loadByEmail(EmailAddress(info.email), 
        Some({_ => throw new PublicException("User.emailExists", info.email)}))
      
    DB.withTransaction(dbName(System)) { implicit conn =>
      // Okay, seems to be legit
      val userId = OID.next(System)
      // TODO: we should have a standardized utility to deal with this
      val timestamp = org.joda.time.DateTime.now()
      val userInsert = SQL("""
          INSERT User
            (id, level, join_date)
            VALUES
            ({userId}, {level}, {now})
          """).on("userId" -> userId.raw, "level" -> UserLevel.PendingUser, "now" -> timestamp.toDate())
      // TBD: we *should* be checking the return value here, but it is spuriously returning false. Why?
      userInsert.execute
//      if (!userInsert.execute)
//        throw new Exception("Unable to create new User!")
      val identityId = OID.next(System)
      val identityInsert = SQL("""
          INSERT Identity
            (id, name, userId, kind, handle, email, authentication)
            VALUES
            ({identityId}, {display}, {userId}, {kind}, {handle}, {email}, {authentication})
          """).on(
            "identityId" -> identityId.raw,
            "display" -> info.display,
            "userId" -> userId.raw,
            "kind" -> IdentityKind.QuerkiLogin,
            "handle" -> info.handle,
            "email" -> info.email,
            "authentication" -> Hasher.calcHash(info.password).toString)
        identityInsert.execute
//      if (!identityInsert.execute)
//        throw new Exception("Unable to create new Identity!")
    }
    
    // Finally, make sure that things load correctly
    // TBD: this fails if I try to do it in the same transaction. Why?
    checkQuerkiLogin(info.handle, info.password).getOrElse(throw new Exception("Unable to load newly-created Identity!"))
  }
  
  def changePassword(requester:User, identity:Identity, newPassword:String):Try[User] = Try {
    if (!requester.isAdmin && !requester.hasIdentity(identity.id))
      throw new Exception("Illegal attempt to change password!")
    
    DB.withTransaction(dbName(System)) { implicit conn =>
      val update = SQL("""
          UPDATE Identity
             SET authentication = {authentication}
           WHERE id = {id}
          """).on(
            "authentication" -> Hasher.calcHash(newPassword).toString,
            "id" -> identity.id.raw
              )
       update.executeUpdate
    }
    
    checkQuerkiLogin(identity.handle, newPassword).getOrElse(throw new Exception("Unable to load newly-created Identity!"))
  }
  
  def addSpaceMembership(identityId:OID, spaceId:OID):Boolean = {
    DB.withConnection(dbName(System)) { implicit conn =>
      val insert = SQL("""
          INSERT SpaceMembership
            (identityId, spaceId)
            VALUES
            ({identityId}, {spaceId})
          """).on("identityId" -> identityId.raw, "spaceId" -> spaceId.raw)
      insert.execute
    }
  }
  
  def changeUserLevel(userId:OID, requester:User, level:UserLevel):Option[User] = requester.requireAdmin {
    DB.withConnection(dbName(System)) { implicit conn =>
      val update = SQL("""
          UPDATE User
             SET level={lv}
           WHERE id={userId}
          """).on("lv" -> level, "userId" -> userId.raw)
      update.executeUpdate
      
      val userQuery = userLoadSqlWhere("""User.id={userId}""").on("userId" -> userId.raw)
      getUser(userQuery)
    }
  }

}

object IdentityKind {
  val SimpleEmail = 1
  val QuerkiLogin = 2
  
  type IdentityKind = Int
}
import IdentityKind._

case class Identity(id:OID, email:EmailAddress, auth:String, handle:String, name:String, kind:IdentityKind)

object Identity {
  // TODO: this shouldn't be synchronous! There's a DB call in it, so it should be async.
  // DEPRECATED: this was for the old Space-Only users for the Wedding Space
  def getOrCreateByEmail(email:EmailAddress, name:String):Identity = {
    DB.withConnection(dbName(System)) { implicit conn =>
      val identityQuery = SQL("""
          SELECT * FROM Identity 
           WHERE email={email}
             AND kind={kind}
          """).on("email" -> email.addr, "kind" -> IdentityKind.SimpleEmail)
      val stream = identityQuery.apply()
      stream.headOption.map(row => Identity(row.oid("id"), email, "", "", "", SimpleEmail)).getOrElse {
        val identityId = OID.next(ShardKind.System)
        SQL("""
            INSERT INTO Identity
            (id, name, kind, email) VALUES
            ({id}, {name}, {kind}, {email})
            """).on("id" -> identityId.raw, "name" -> name, "kind" -> IdentityKind.SimpleEmail, "email" -> email.addr).executeUpdate()
        Identity(identityId, email, "", "", "", SimpleEmail)
      }
    }
  }
}