package controllers

import play.api._
import play.api.data._
import play.api.data.Forms._
import play.api.mvc._

import models._

object Application extends Controller {

  val userForm = Form(
    mapping(
      "name" -> nonEmptyText
    )((name) => User(UnknownOID, name))
     ((user: User) => Some((user.name)))
  )
  
  def getUser(request:Request[_]):Option[User] = {
	request.cookies.get("username").flatMap(cookie => User.get(cookie.value))
  }
  
  /**
   * Utility method for pages that are basically displaying some aspect of a whole Space.
   * 
   * @param request The incoming page request
   * @param spaceId The stringified OID of the space to display, from the URL
   * @param action A callback that, given the user and the space's state, produces the
   * actual page content
   */
  // TODO: check authorization!
  def withSpace(request:Request[_], spaceId:String, action:(Option[User], SpaceState) => SimpleResult[_]) = {
    val user = getUser(request)
    Async {
      SpaceManager.ask[GetSpaceResponse, Result](GetSpace(OID(spaceId))) {
        case RequestedSpace(state) => action(user, state)
        case GetSpaceFailed(id) => Ok(views.html.spaces(getUser(request), Seq.empty))
      }
    }    
  }
  
  def index = Action { request =>
    // TODO: this doesn't need the Async test code any more:
    Async {
	    SpaceManager.ask[String,Result](SaySomething("Why, hello")) { mgrResp =>
	      Ok(views.html.index(getUser(request), userForm))      
	    }      
    }
  }
  
  // TODO: require login!
  def spaces = Action { request =>
    val user = getUser(request)
    if (user.isEmpty)
      BadRequest(views.html.index(None, userForm, Some("You aren't logged in")))
    else Async {
      SpaceManager.ask[ListMySpacesResponse, Result](ListMySpaces(user.get.id)) { 
        case MySpaces(list) => Ok(views.html.spaces(user, list))
      }
    }
  }
  
  def space(spaceId:String) = Action { request =>
    withSpace(request, spaceId, (user, state) => Ok(views.html.thing(user, state, state)))
  }
  
  def things(spaceId:String) = Action { request =>
    withSpace(request, spaceId, (user, state) => Ok(views.html.things(user, state)))
  }
  
  def thing(spaceId:String, thingId:String) = Action { request =>
    withSpace(request, spaceId, (user, state) => {
      Ok(views.html.thing(user, state, state.anything(OID(thingId))))
    })
  }
  
  def login = Action { implicit request =>
    userForm.bindFromRequest.fold(
      errors => BadRequest(views.html.index(None, errors, Some("I didn't understand that"))),
      user => {
        val lookedUp = User.get(user.name)
        if (lookedUp.isEmpty)
          BadRequest(views.html.index(None, userForm, Some("I don't know who you are")))
        else
	      Redirect(routes.Application.index).withCookies(Cookie("username", user.name))
      }
    )
  }
  
  def logout = Action {
    Redirect(routes.Application.index).discardingCookies("username")
  }
}