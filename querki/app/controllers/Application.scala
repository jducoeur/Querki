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
    )(User.apply)(User.unapply)
  )
  
  def getUser(request:Request[_]):Option[User] = {
	request.cookies.get("username").map(cookie => User(cookie.value))
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
        case RequestedSpace(state) => action(user, state) //Ok(views.html.space(getUser(request), state))
        case GetSpaceFailed(id) => Ok(views.html.spaces(getUser(request), Seq.empty))
      }
    }    
  }
  
  def index = Action { request =>
    Async {
	    SpaceManager.ask[String,Result](SaySomething("Why, hello")) { mgrResp =>
	      Ok(views.html.index(getUser(request), userForm, Some(mgrResp)))      
	    }      
    }
  }
  
  // TODO: require login!
  def spaces = Action { request =>
    Async {
      SpaceManager.ask[ListMySpacesResponse, Result](ListMySpaces(system.SystemSpace.SystemUserOID)) { 
        case MySpaces(list) => Ok(views.html.spaces(getUser(request), list))
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
	    Redirect(routes.Application.index).withCookies(Cookie("username", user.name))
      }
    )
  }
  
  def logout = Action {
    Redirect(routes.Application.index).discardingCookies("username")
  }
}