package controllers

import language.existentials

import javax.inject._

import scala.concurrent.Future

import play.api._
import play.api.data._
import play.api.data.Forms._
import play.api.mvc._

import models.AsName

import querki.spaces.messages._

import querki.util.TryTrans

class Application @Inject() (val appProv:Provider[play.api.Application]) extends ApplicationBase {
  
  val newSpaceForm = Form(
    mapping(
      "name" -> nonEmptyText
    )((name) => name)
     ((name:String) => Some(name))
  )
  
  lazy val Core = interface[querki.core.Core]
  lazy val System = interface[querki.system.System]
  
  def scalajsFile(file: String) = Action {
    Ok.sendFile(new java.io.File(s"scalajs/$file"))
  }

  def scalaSharedFile(file: String) = Action {
    Ok.sendFile(new java.io.File(s"scala/$file"))
  }
  
  def robots = Action { implicit request =>
    Ok("""# We are experimentally allowing the robots in. If this severely harms system performance, we may turn it off again.
        
#user-agent: *
#disallow: /
      
user-agent: facebookexternalhit/1.1
disallow:
allow: /
""").as("text/plain")
  }
}
