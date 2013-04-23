package identity

import anorm._
import play.api._
import play.api.db._
import play.api.Play.current
import models._

case class User(id:OID, name:String, password:String) {
  // TODO: we'll need to cope with users who don't have a name, since that's a
  // paid-user feature. In that case, return a ThingId'ized id.
  def toThingId = AsName(name)
}

object User {
  // TODO: dear Lord, this needs to be cached:
  def get(rawName:String) = {
    val name = system.NameType.canonicalize(rawName)
    DB.withConnection { implicit conn =>
      val personQuery = SQL("""
          select * from User where name={name}
          """).on("name" -> name)
      val stream = personQuery.apply()
      stream.headOption.map(row => User(OID(row.get[Long]("id").get), row.get[String]("name").get, ""))
    }
  }
  
  def getName(id:OID):String = {
    DB.withConnection { implicit conn =>
      val personQuery = SQL("""
          select name from User where id={id}
          """).on("id" -> id.raw)
      val stream = personQuery.apply()
      stream.headOption.map(row => row.get[String]("name").get) getOrElse ("UNKNOWN USER")
    }    
  }
  
    // TODO: the whole login procedure should become very different:
  def checkLogin(name:String, passwordEntered:String):Boolean = {
    val pwdOpt = Play.configuration.getString("querki.test.password." + name)
    pwdOpt map { pwd => if (passwordEntered == pwd) true else false } getOrElse false    
  }
}