package models

import anorm._

import play.api._
import play.api.db._
import play.api.Play.current

case class User(id:OID, name:String, password:String) {
  // TODO: we'll need to cope with users who don't have a name, since that's a
  // paid-user feature. In that case, return a ThingId'ized id.
  def toThingId = AsName(name)
}

object User {
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
  
    // TODO: the whole login procedure should become very different:
  def checkLogin(name:String, passwordEntered:String):Boolean = {
    val pwdOpt = Play.configuration.getString("querki.test.password." + name)
    pwdOpt map { pwd => if (passwordEntered == pwd) true else false } getOrElse false    
  }
}