package controllers

import play.api._
import play.api.data._
import play.api.data.Forms._
import play.api.mvc._

import models.User

object Application extends Controller {

  val userForm = Form(
    mapping(
      "name" -> nonEmptyText
    )(User.apply)(User.unapply)
  )
  
  def index = Action { request =>
    val userNameOpt = request.cookies.get("username")
	val user = userNameOpt match {
	  case None => None
	  case Some(cookie) => Some(User(cookie.value))
	}
    Ok(views.html.index(user, userForm))
  }
  
  def login = Action { implicit request =>
    userForm.bindFromRequest.fold(
      errors => BadRequest(views.html.index(None, errors, Some("I didn't understand that"))),
      user => {
	    Redirect(routes.Application.index).withCookies(Cookie("username", user.name))
      }
    )
  }
  
  def logout = Action {
    Redirect(routes.Application.index).discardingCookies("username")
  }
}