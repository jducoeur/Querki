package querki.identity

import scala.util.Try

import anorm._
import anorm.SqlParser
import SqlParser._
import play.api._
import play.api.db._
import play.api.mvc._

import models.{AsName, AsOID, OID, ThingId, UnknownOID}

import querki.core.NameUtils
import querki.db._
import querki.ecology._
import querki.email.EmailAddress
import querki.globals._
import querki.util.SqlHelpers._

import UserLevel._

object UserMOIDs extends EcotIds(30)

class UserPersistence(e:Ecology) extends QuerkiEcot(e) with UserAccess {
  
  lazy val Encryption = interface[querki.security.Encryption]
  lazy val IdentityAccess = interface[IdentityAccess]
  lazy val UserCacheAccess = interface[UserCacheAccess]
  
  /**
   * This is used in various queries -- it is a new (Play 2.4) style parser from a row to a User.
   */
  private val userParser:RowParser[User] =
    for {
      email <- str("email")
      identityOID <- oid("id")
      userOID <- oid("userId")
      name <- str("name")
      auth <- str("authentication")
      // TODO: handle really should be an Option[String]!
      handle <- SqlParser.get[Option[String]]("handle").map(_.getOrElse(""))
      level <- int("level")
      tosV <- int("tosVersion")
    }
      yield 
        FullUser(
          userOID, name,
          Seq(Identity(identityOID, EmailAddress(email), auth, handle, name, IdentityKind.QuerkiLogin)),
          level,
          tosV
        )
  
  private def getUser(query:SimpleSql[Row], checkOpt:Option[User => Boolean] = None):Option[User] = {
    QDB(ShardKind.System) { implicit conn =>
      query
        .as(userParser.singleOpt)
        .flatMap { user =>
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
    val handle = NameUtils.canonicalize(rawHandle)
    val personQuery = userLoadSqlWhere("""handle={handle} and kind={loginKind}""").on("handle" -> handle, "loginKind" -> IdentityKind.QuerkiLogin)
    getUser(personQuery, checkOpt)
  }
  
  private def loadByUserId(userId:OID):Option[User] = {      
    val userQuery = userLoadSqlWhere("""User.id={userId}""").on("userId" -> userId.raw)
    getUser(userQuery)
  }
  
  def getByUserId(requester:User, userId:OID):Option[User] = requester.requireAdmin {
    loadByUserId(userId)
  }
  
  // DEPRECATED: we need to move away from this
  def getAllForAdmin(requester:User):Seq[User] = requester.requireAdmin {
    QDB(ShardKind.System) { implicit conn =>
      userLoadSqlWhere("""User.level != 0""").as(userParser.*)
    }
  }
  
  // DEPRECATED: does this even make sense after Open Beta? Probably not.
  def getPendingForAdmin(requester:User):Seq[User] = requester.requireAdmin {
    QDB(ShardKind.System) { implicit conn =>
      userLoadSqlWhere("""User.level = 1""").as(userParser.*)
    }
  }
  
  // DEPRECATED: this is obvious evil. Where are we using it?
  def getAllIdsForAdmin(requester:User):Seq[UserId] = requester.requireAdmin {
    QDB(ShardKind.System) { implicit conn =>
      SQL("""
          SELECT id
            FROM User
          """)
        .as(oid("id").*)
    }    
  }
  
  /**
   * If we find the username in the current session, return a populated Some(User); otherwise, None.
   * 
   * TODO: cache the full record in the cookie! Note that this is closely related to User.toSession().
   * 
   * TODO: remove this -- it is moving into Session instead.
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
  
  def getUserByHandle(handle:String):Option[User] = loadByHandle(handle, None)
  
  def getUserByHandleOrEmail(raw:String):Option[User] = {
    if (raw.contains("@"))
      loadByEmail(EmailAddress(raw), None)
    else
      loadByHandle(raw, None)
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
        val identityOpt = user.identityBy(_.email.addr.toLowerCase() == email.addr.toLowerCase())
        identityOpt.map(identity => Encryption.authenticate(passwordEntered, identity.auth)).getOrElse(false) 
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
          identityOpt.map(identity => Encryption.authenticate(passwordEntered, identity.auth)).getOrElse(false) 
        })
      )
    }
  }

  /**
   * Creates a user based on the given information. Gets created as Pending or Free depending on
   * whether the email address is confirmed. (Different pathways get here in different ways.)
   */
  def createUser(info:SignupInfo, confirmedEmail:Boolean, identityIdOpt:Option[OID] = None):Try[User] = Try {
    // Note that both of these will either return None or throw an exception.
    // We intentionally check email first, to catch the case where you've already created an
    // account and have forgotten about it.
    val emailOpt = loadByEmail(EmailAddress(info.email), 
        Some({_ => throw new PublicException("User.emailExists", info.email)}))
    val existingOpt = loadByHandle(info.handle, 
        Some({_ => throw new PublicException("User.handleExists", info.handle)}))
    // Belt and suspenders checks if this claims to be a SimpleEmail Identity that we are upgrading:
    identityIdOpt.map { identityId =>
      getFullIdentity(identityId) match {
        case Some(identity) =>
          if (identity.kind != IdentityKind.SimpleEmail) {
            QLog.logAndThrowException(new Exception(s"Somehow attempting to upgrade an Identity that already has a User!"))
          }
        case None => {
          QLog.logAndThrowException(new Exception(s"Somehow trying to createUser for an unknown Identity $identityId!"))
        }
      }
    }
        
    val level =
      if (confirmedEmail)
        UserLevel.FreeUser
      else
        UserLevel.PendingUser
    val userId = OID.next(ShardKind.System)
    val timestamp = org.joda.time.DateTime.now()
    val authentication = Encryption.calcHash(info.password)
      
    QDB(ShardKind.System) { implicit conn =>
      // Okay, seems to be legit
      val userInsert = SQL("""
          INSERT User
            (id, level, join_date)
            VALUES
            ({userId}, {level}, {now})
          """).on("userId" -> userId.raw, "level" -> level, "now" -> timestamp.toDate())
      // TBD: we *should* be checking the return value here, but it is spuriously returning false. Why?
      userInsert.execute
      
      identityIdOpt match {
        case Some(identityId) => {
          // We're upgrading an existing Identity.
          QLog.spew(s"Upgrading an existing SimpleEmail Identity")
          val identityUpdate = SQL("""
              UPDATE Identity
                 SET  name = {display},
                    userId = {userId},
                      kind = {kind},
                    handle = {handle},
                     email = {email},
            authentication = {authentication}
               WHERE    id = {id}
            """).on(
              "id" -> identityId.raw,
              "display" -> info.display,
              "userId" -> userId.raw,
              "kind" -> IdentityKind.QuerkiLogin,
              "handle" -> info.handle,
              "email" -> info.email,
              "authentication" -> authentication
            )
          identityUpdate.executeUpdate()
        }
        case _ => {
          // There is no pre-existing Identity, so create it from scratch:
          QLog.spew(s"Creating a brand new Identity")
          val identityId = OID.next(ShardKind.System)
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
                "authentication" -> authentication)
          identityInsert.execute
        }
      }
    }
    
    // Finally, make sure that things load correctly
    // TBD: this fails if I try to do it in the same transaction. Why?
    checkQuerkiLogin(info.handle, info.password).getOrElse(throw new Exception("Unable to load newly-created Identity!"))
  }
  
  private val identityParser:RowParser[FullIdentity] =
    for {
      email <- str("email")
      identityOID <- oid("id")
      userOID <- oid("userId")
      name <- str("name")
      handle <- SqlParser.get[Option[String]]("handle").map(_.getOrElse(""))
      kind <- int("kind")
    }
      yield 
        FullIdentity(identityOID, EmailAddress(email), handle, name, userOID, kind)
      
  def getFullIdentity(id:IdentityId):Option[FullIdentity] = {
    val query = SQL("""
            SELECT id, name, userId, authentication, email, handle, kind
              FROM Identity
            WHERE id = {id}""")
          .on("id" -> id.raw)
    QDB(ShardKind.System) { implicit conn =>
      query.as(identityParser.singleOpt)
    }
  }
  
  def findOrCreateIdentityByEmail(emailIn:String):FullIdentity = {
    // Make sure that email gets normalized:
    val email = emailIn.toLowerCase()
    val query = SQL("""
            SELECT id, name, userId, authentication, email, handle, kind
              FROM Identity
            WHERE email = {email}""")
          .on("email" -> email)
    QDB(ShardKind.System) { implicit conn =>
      query.as(identityParser.singleOpt) match {
        // We found an Identity with that email address:
        case Some(identity) => identity
        // There isn't one, so create a trivial Identity so we can send and track emails to it:
        case None => {
          val identityId = OID.next(ShardKind.System)
          // For the preliminary display name, we just use whatever comes before the @
          val displayName = email.takeWhile(_ != '@')
          val identityInsert = SQL("""
              INSERT Identity
                (id, name, userId, kind, handle, email, authentication)
                VALUES
                ({identityId}, {display}, {userId}, {kind}, {handle}, {email}, {authentication})
              """).on(
                "identityId" -> identityId.raw,
                "display" -> displayName,
                "userId" -> UnknownOID.raw,
                "kind" -> IdentityKind.SimpleEmail,
                // There is no handle for the time being:
                "handle" -> "",
                "email" -> email,
                "authentication" -> "")
          identityInsert.execute
          FullIdentity(identityId, EmailAddress(email), "", displayName, UnknownOID, IdentityKind.SimpleEmail)
        }
      }
    }
  }
  
  def changePassword(requester:User, identity:Identity, newPassword:String):Try[User] = Try {
    if (!requester.isAdmin && !requester.hasIdentity(identity.id))
      throw new Exception("Illegal attempt to change password!")
    
    QDB(ShardKind.System) { implicit conn =>
      val update = SQL("""
          UPDATE Identity
             SET authentication = {authentication}
           WHERE id = {id}
          """).on(
            "authentication" -> Encryption.calcHash(newPassword),
            "id" -> identity.id.raw
              )
       update.executeUpdate
    }
    
    checkQuerkiLogin(identity.handle, newPassword).getOrElse(throw new Exception("Unable to load newly-created Identity!"))
  }
  
  def changeDisplayName(requester:User, identity:Identity, newDisplay:String):Future[User] = {
    if (!requester.isAdmin && !requester.hasIdentity(identity.id))
      throw new Exception("Illegal attempt to change password!")
    
    QDB(ShardKind.System) { implicit conn =>
      val update = SQL("""
          UPDATE Identity
             SET name = {display}
           WHERE id = {id}
          """).on(
            "display" -> newDisplay,
            "id" -> identity.id.raw
              )
       update.executeUpdate
    }
    
    // Tell the cache to reload at the next opportunity:
    IdentityAccess.invalidateCache(identity.id)
    
    val user = getUserForIdentity(identity.id).getOrElse(throw new Exception("Unable to reload user record!"))
    updateUserCacheFor(Some(user)).map(_.get)
  }
  
  def addSpaceMembership(identityId:OID, spaceId:OID, membershipState:MembershipState):Boolean = {
    QDB(ShardKind.System) { implicit conn =>
      val insert = SQL("""
          INSERT SpaceMembership
            (identityId, spaceId, membershipState)
            VALUES
            ({identityId}, {spaceId}, {membershipState})
          """).on("identityId" -> identityId.raw, "spaceId" -> spaceId.raw, "membershipState" -> membershipState)
      insert.execute
    }
  }
  
  private def updateUserCacheFor(userOpt:Option[User]) = {
    userOpt match {
      case Some(user) => UserCacheAccess.updateCacheAndThen(user).map( _ => Some(user))
      case None => Future.successful(None)
    }    
  }
  
  def changeUserLevel(userId:OID, requester:User, level:UserLevel):Future[Option[User]] = requester.requireAdmin {
    QDB(ShardKind.System) { implicit conn =>
      val update = SQL("""
          UPDATE User
             SET level={lv}
           WHERE id={userId}
          """).on("lv" -> level, "userId" -> userId.raw)
      update.executeUpdate
    }
    
    val userOpt = QDB(ShardKind.System) { implicit conn => loadByUserId(userId) }
    
    updateUserCacheFor(userOpt)
  }

  def setTOSVersion(userId:OID, version:Int) = {
    QDB(ShardKind.System) { implicit conn =>
      val update = SQL("""
          UPDATE User
             SET tosVersion={v}
           WHERE id={userId}
          """).on("v" -> version, "userId" -> userId.raw)
      update.executeUpdate
    }
    
    val userOpt = QDB(ShardKind.System) { implicit conn => loadByUserId(userId) }
    
    updateUserCacheFor(userOpt)
  }
  
  // TODO: in the long run, this query is just plain too heavy. We probably need something more efficient. We
  // may have to denormalize this into the Akka Persistence level.
  def getAcquaintanceIds(identityId:IdentityId):Seq[IdentityId] = {
    QDB(ShardKind.System) { implicit conn =>
      SQL("""
          SELECT DISTINCT OtherMember.identityId FROM SpaceMembership
            JOIN SpaceMembership AS OtherMember ON OtherMember.spaceId = SpaceMembership.spaceId
           WHERE SpaceMembership.identityId = {identityId}
             AND OtherMember.identityId != {identityId}
          """)
        .on("identityId" -> identityId.raw)
        .as(oid("identityId").*)
    }
  }
  
  def getUserVersion(userId:UserId):Option[Int] = {
    QDB(ShardKind.System) { implicit conn =>
      SQL("""
          SELECT userVersion from User
           WHERE id = {id} 
          """)
        .on("id" -> userId.raw)
        .as(int("userVersion").singleOpt)
    }    
  }
}