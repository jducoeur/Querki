package querki.identity

import anorm._
import play.api._
import play.api.db._
import play.api.mvc._
import play.api.Play.current
import models._

import querki.db.ShardKind
import ShardKind._

import querki.util._

import modules.email.EmailAddress

trait User {
  def id:OID
  // TODO: this should be Option[String]!
  def name:String
  def identities:Seq[Identity]
   
  def toSession:Seq[(String, String)] = {
    Seq(
      (Security.username -> name),
      (User.userIdSessionParam -> id.toString)
    )
  }
  
  // TODO: we'll need to cope with users who don't have a name, since that's a
  // paid-user feature. In that case, return a ThingId'ized id.
  def toThingId = AsName(name)
}

case class FullUser(id:OID, name:String, identities:Seq[Identity] = Seq.empty) extends User

object UserLevel {
  type UserLevel = Int
  
  val UnknownUserLevel = 0
  val PendingUser = 1
  val FreeUser = 2
  val PaidUser = 3
  val PermanentUser = 4
  
  val AdminUser = 10
  
  val SuperadminUser = 100
}

object User {
  val userIdSessionParam = "userId"
    
  object Anonymous extends User {
    val id = UnknownOID
    val name = ""
    val identities = Seq.empty
  }
  
  /**
   * We look users up by name pretty frequently, so I'm going to tentatively put a cache here,
   * just to reduce DB traffic.
   * 
   * TODO: this really ought to age out. I don't expect entries to be removed often -- indeed, I
   * expect users to release names quite rarely -- but we should be able to cope with it.
   */
  private val idByNameCache = collection.mutable.Map.empty[String, OID]
  
  /**
   * If we find the username in the current session, return a populated Some(User); otherwise, None.
   */
  def get(request:RequestHeader) = {
    val username = request.session.get(Security.username)
    username.map(FullUser(request.session.get(userIdSessionParam).map(OID(_)).getOrElse(UnknownOID), _))
  }
  
  def get(rawName:String) = {
    val name = system.NameType.canonicalize(rawName)
    val idOpt = idByNameCache.get(name).orElse {
      // Don't have it cached, so fetch the ID and cache it:
      val fromDB = DB.withConnection(dbName(System)) { implicit conn =>
        val personQuery = SQL("""
          select id from User where name={name}
          """).on("name" -> name)
        val stream = personQuery.apply()
        // TODO: need to cope 
        stream.headOption.map(row => OID(row.get[Long]("id").get))
      }
      fromDB.map(idByNameCache(name) = _)
      fromDB
    }
    idOpt.map(FullUser(_, name))
  }
  
  def getName(id:OID):String = {
    DB.withConnection(dbName(System)) { implicit conn =>
      val personQuery = SQL("""
          select name from User where id={id}
          """).on("id" -> id.raw)
      val stream = personQuery.apply()
      stream.headOption.map(row => row.get[String]("name").get) getOrElse ("UNKNOWN USER")
    }    
  }
  
  def checkQuerkiLogin(email:EmailAddress, passwordEntered:String):Option[User] = {
    DB.withConnection(dbName(System)) { implicit conn =>
      val identityQuery = SQL("""
          SELECT Identity.id, Identity.name, userId, User.level, authentication FROM Identity
            JOIN User ON User.id=userId
            WHERE email={email};
          """).on("email" -> email.addr.toLowerCase())()
      identityQuery.headOption.flatMap { row =>
        val auth = row.get[String]("authentication").get
        if (Hasher.authenticate(passwordEntered, EncryptedHash(auth))) {
          Some(FullUser(OID(row.get[Long]("userId").get), row.get[String]("name").get,
            Seq(Identity(OID(row.get[Long]("id").get), email))))
        } else {
          None
        }
      }
    }
  }
  
  def checkQuerkiLogin(login:String, passwordEntered:String):Option[User] = {
    if (login.contains("@")) {
      checkQuerkiLogin(EmailAddress(login), passwordEntered)
    } else {
      DB.withConnection(dbName(System)) { implicit conn =>
        val identityQuery = SQL("""
            SELECT Identity.id, Identity.name, userId, User.level, authentication, email FROM Identity
              JOIN User ON User.id=userId
              WHERE Identity.name={name};
            """).on("name" -> login.toLowerCase())()
        identityQuery.headOption.flatMap { row =>
          val auth = row.get[String]("authentication").get
          if (Hasher.authenticate(passwordEntered, EncryptedHash(auth))) {
            Some(FullUser(OID(row.get[Long]("userId").get), row.get[String]("name").get,
              Seq(Identity(OID(row.get[Long]("id").get), EmailAddress(row.get[String]("email").get)))))
          } else {
            None
          }
        }
      }
    }
  }
}

case class Identity(id:OID, email:EmailAddress)

object IdentityKind {
  val SimpleEmail = 1
  val QuerkiLogin = 2
  
  type IdentityKind = Int
}

object Identity {
  def getOrCreateByEmail(email:EmailAddress, name:String):Identity = {
    DB.withConnection(dbName(System)) { implicit conn =>
      val identityQuery = SQL("""
          SELECT * FROM Identity 
           WHERE email={email}
             AND kind={kind}
          """).on("email" -> email.addr, "kind" -> IdentityKind.SimpleEmail)
      val stream = identityQuery.apply()
      stream.headOption.map(row => Identity(OID(row.get[Long]("id").get), email)).getOrElse {
        val identityId = OID.next(ShardKind.System)
        SQL("""
            INSERT INTO Identity
            (id, name, kind, email) VALUES
            ({id}, {name}, {kind}, {email})
            """).on("id" -> identityId.raw, "name" -> name, "kind" -> IdentityKind.SimpleEmail, "email" -> email.addr).executeUpdate()
        Identity(identityId, email)
      }
    }
  }
}