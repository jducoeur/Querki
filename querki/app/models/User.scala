package models

import anorm._

import play.api._
import play.api.db._
import play.api.Play.current

case class User(id:OID, name:String)

object User {
  def get(name:String) = {
    DB.withConnection { implicit conn =>
      val personQuery = SQL("""
          select * from User where name={name}
          """).on("name" -> name)
      Logger.info("Looking up " + personQuery)
      val stream = personQuery.apply()
      stream.headOption.map(row => User(OID(row.get[Long]("id").get), row.get[String]("name").get))
    }
  }
}