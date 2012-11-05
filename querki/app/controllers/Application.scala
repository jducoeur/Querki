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
  def index = Action {
    Ok(views.html.index(None, userForm))
  }
  
  def login = Action { implicit request =>
    userForm.bindFromRequest.fold(
      errors => BadRequest(views.html.index(None, errors, "I didn't understand that")),
      user => {
	    Ok(views.html.index(Some(user), userForm))
      }
    )
  }
}