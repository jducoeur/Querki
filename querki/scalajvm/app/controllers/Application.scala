package controllers

import language.existentials

import scala.concurrent.Future

import play.api._
import play.api.data._
import play.api.data.Forms._
import play.api.mvc._

import models.AsName

import querki.spaces.messages._

import querki.util.TryTrans

class Application extends ApplicationBase {
  
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
  
  // TODO: in the long run, we will want spidering of the main system pages, and allow users to
  // choose to have their pages indexed. We're quite a ways from that, though.
  def robots = Action { implicit request =>
    Ok("""# For the time being, Querki requests that robots stay out. This will change eventually.
        
user-agent: *
disallow: /
""").as("text/plain")
  }
}
