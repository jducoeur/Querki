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
}

import UserLevel._

trait User {
  def id:OID
  // TODO: this should be Option[String]!
  def name:String
  def identities:Seq[Identity]
  def level:UserLevel
  
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
}

case class FullUser(id:OID, name:String, identities:Seq[Identity] = Seq.empty, level:UserLevel = UnknownUserLevel) extends User

object SqlHelpers {
  /**
   * A simple pimped type to make SqlRow slightly less unpleasant to use.
   * 
   * Note that these methods will all throw exceptions if the column isn't found! This is not designed
   * to be gentle if the code and DB have somehow gotten out of sync.
   */
  implicit class EnhancedSqlRow(row:SqlRow) {
    def string(name:String) = row.get[String](name).get
    def oid(name:String) = OID(row.get[Long](name).get)
    def int(name:String) = row.get[Int](name).get
  }
}
import SqlHelpers._

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
   * The in-memory cache of Users. We use this *really* frequently, so it's helpful to have it
   * cached.
   * 
   * TODO: these caches, together, probably aren't quite thread-safe. And the resulting code
   * is too incestuous.
   * 
   * TODO: this needs mechanisms for signaling when an entry is obsolete, that work cross-cluster.
   * Really, this cache is a bit dangerous, and may need to be replaced by something
   * very different. (Eg, hidden behind an Actor that gets queried.)
   */
  private var userByIdCache = Map.empty[OID, User]
  private var idByEmailCache = Map.empty[String, OID]
  private var userByIdentityCache = Map.empty[OID, OID]
  
  private def getUser(query:Sql, checkOpt:Option[User => Boolean] = None):Option[User] = {
    // TODO: check the cache? Or should that be done at the semantic levels below?
    // There's a case to be made that the cache check is necessarily tied to the query,
    // and therefore just plain doesn't belong here...
    DB.withConnection(dbName(System)) { implicit conn =>
      val result = query()
      result.headOption.flatMap { row =>
        val email = EmailAddress(row.string("email"))
        val identityOID = row.oid("id")
        // Create the User record
        val user = FullUser(row.oid("userId"), row.string("name"),
          Seq(Identity(identityOID, email, row.string("authentication"), row.string("handle"), row.string("name"), IdentityKind.QuerkiLogin)),
          row.int("level"))
          
        // TODO: Stuff it into the cache? Or should that be done at the semantic levels below?
        userByIdCache = userByIdCache + (user.id -> user)
        idByEmailCache = idByEmailCache + (email.addr -> user.id)
        userByIdentityCache = userByIdentityCache + (identityOID -> user.id)
        
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
        SELECT Identity.id, Identity.name, userId, User.level, authentication, email, handle FROM Identity
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
   * If we find the username in the current session, return a populated Some(User); otherwise, None.
   * 
   * TODO: cache the full record in the cookie! Note that this is closely related to User.toSession().
   */
  def get(request:RequestHeader) = {
    val username = request.session.get(Security.username)
    username.flatMap(loadByHandle(_, None))
//      FullUser(
//        request.session.get(userIdSessionParam).map(OID(_)).getOrElse(UnknownOID), 
//        _,
//        Seq.empty,
//        request.session.get(levelSessionParam).map(_.toInt).getOrElse(UnknownUserLevel)))
  }
  
  // TODO: this shouldn't be synchronous! There's a DB call in it, so it should be async.
  def get(rawName:String) = {
    val name = system.NameType.canonicalize(rawName)
    val idOpt = idByNameCache.get(name).orElse {
      // Don't have it cached, so fetch the ID and cache it:
      val fromDB = DB.withConnection(dbName(System)) { implicit conn =>
        val personQuery = SQL("""
          select id from User where name={name}
          """).on("name" -> name)
        val stream = personQuery.apply()
        stream.headOption.map(row => OID(row.get[Long]("id").get))
      }
      fromDB.map(idByNameCache(name) = _)
      fromDB
    }
    idOpt.map(FullUser(_, name))
  }
 
  // TODO: this shouldn't be synchronous! There's a DB call in it, so it should be async.
  def getIdentity(rawHandle:String) = {
    loadByHandle(rawHandle, None).flatMap(_.identityByHandle(rawHandle)).map(_.id)
//    val handle = system.NameType.canonicalize(rawHandle)
//    val personQuery = userLoadSqlWhere("""handle={name} and kind={loginKind}""").on("handle" -> handle, "loginKind" -> IdentityKind.QuerkiLogin)
//    getUser(personQuery).flatMap(_.identityByHandle(handle)).map(_.id)
//    
//    
//    val idOpt = idByNameCache.get(name).orElse {
//      // Don't have it cached, so fetch the ID and cache it:
//      val fromDB = DB.withConnection(dbName(System)) { implicit conn =>
//        val personQuery = SQL("""
//          select id from Identity where handle={name} and kind={loginKind}
//          """).on("name" -> name, "loginKind" -> IdentityKind.QuerkiLogin)
//        val stream = personQuery.apply()
//        stream.headOption.map(row => OID(row.get[Long]("id").get))
//      }
//      fromDB.map(idByNameCache(name) = _)
//      fromDB
//    }
//    idOpt
  }

  /**
   * Fetched the handle for the Identity matching the given OID. If there isn't one, simply
   * returns the input parameter, ThingId'ed. So it always returns *something* that at least
   * *looks* like a valid ThingId.
   */
  // TODO: this shouldn't be synchronous! There's a DB call in it, so it should be async.
  def getHandle(id:OID):String = {
    // TODO: check the userByIdentityCache
    val query = userLoadSqlWhere("Identity.id={id}").on("id" -> id.raw)
    getUser(query).flatMap(_.identityById(id)).map(_.handle).getOrElse(id.toThingId.toString)
//    
//    
//    nameByIdCache.get(id).getOrElse {
//      val fromDB = DB.withConnection(dbName(System)) { implicit conn =>
//        val personQuery = SQL("""
//            select handle from Identity where id={id}
//            """).on("id" -> id.raw)
//        val stream = personQuery.apply()
//        stream.headOption.map(row => row.get[String]("handle").get) getOrElse (id.toThingId.toString)
//      }
//      nameByIdCache(id) = fromDB
//      fromDB
//    }
  }
  
  // Note that this assumes that the ID identifies a valid Identity.
  // TODO: this shouldn't be synchronous! There's a DB call in it, so it should be async.
  def getName(id:OID):String = {
    val query = userLoadSqlWhere("id={id}").on("id" -> id.raw)
    getUser(query).flatMap(_.identityById(id)).map(_.name).getOrElse(id.toThingId.toString)
//    
//    DB.withConnection(dbName(System)) { implicit conn =>
//      val personQuery = SQL("""
//          select name from Identity where id={id}
//          """).on("id" -> id.raw)
//      val stream = personQuery.apply()
//      stream.headOption.map(row => row.get[String]("name").get) getOrElse (id.toThingId.toString)
//    }
  }
  
  // TODO: this shouldn't be synchronous! There's a DB call in it, so it should be async.
  def checkQuerkiLogin(email:EmailAddress, passwordEntered:String):Option[User] = {
    loadByEmail(
      email, 
      Some({ user:User => 
        val identityOpt = user.identityBy(_.email.addr == email.addr)
        identityOpt.map(identity => Hasher.authenticate(passwordEntered, EncryptedHash(identity.auth))).getOrElse(false) 
      })
    )
//    
//    
//    DB.withConnection(dbName(System)) { implicit conn =>
//      val identityQuery = SQL("""
//          SELECT Identity.id, Identity.name, userId, User.level, authentication FROM Identity
//            JOIN User ON User.id=userId
//            WHERE email={email};
//          """).on("email" -> email.addr.toLowerCase())()
//      identityQuery.headOption.flatMap { row =>
//        val auth = row.get[String]("authentication").get
//        if (Hasher.authenticate(passwordEntered, EncryptedHash(auth))) {
//          Some(FullUser(OID(row.get[Long]("userId").get), row.get[String]("name").get,
//            Seq(Identity(OID(row.get[Long]("id").get), email)),
//            row.get[Int]("level").get))
//        } else {
//          None
//        }
//      }
//    }
  }
  
  // TODO: this shouldn't be synchronous! There's a DB call in it, so it should be async.
  def checkQuerkiLogin(login:String, passwordEntered:String):Option[User] = {
    if (login.contains("@")) {
      checkQuerkiLogin(EmailAddress(login), passwordEntered)
    } else {
      loadByHandle(
        login, 
        Some({ user:User => 
          val identityOpt = user.identityByHandle(login)
          identityOpt.map(identity => Hasher.authenticate(passwordEntered, EncryptedHash(identity.auth))).getOrElse(false) 
        })
      )
//      
//      
//      
//      
//      DB.withConnection(dbName(System)) { implicit conn =>
//        val identityQuery = SQL("""
//            SELECT Identity.id, Identity.name, userId, User.level, authentication, email FROM Identity
//              JOIN User ON User.id=userId
//              WHERE Identity.handle={handle};
//            """).on("handle" -> login.toLowerCase())()
//        identityQuery.headOption.flatMap { row =>
//          val auth = row.get[String]("authentication").get
//          if (Hasher.authenticate(passwordEntered, EncryptedHash(auth))) {
//            Some(FullUser(OID(row.get[Long]("userId").get), row.get[String]("name").get,
//              Seq(Identity(OID(row.get[Long]("id").get), EmailAddress(row.get[String]("email").get))),
//              row.get[Int]("level").get))
//          } else {
//            None
//          }
//        }
//      }
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