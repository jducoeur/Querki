package querki.identity

import anorm._
import play.api._
import play.api.db._
import play.api.mvc._
import play.api.Play.current
import models._

import querki.db.ShardKind
import ShardKind._

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
  
  // TODO: this should go through the DB fairly soon:
  def checkLogin(rawName:String, passwordEntered:String):Option[User] = {
    val name = system.NameType.canonicalize(rawName)
    val pwdOpt = Play.configuration.getString("querki.test.password." + name)
    pwdOpt map { pwd => if (passwordEntered == pwd) get(name) else None } getOrElse None    
  }
}

import modules.email.EmailAddress

case class Identity(id:OID, email:EmailAddress)

object IdentityKind {
  val SimpleEmail = 1
  
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