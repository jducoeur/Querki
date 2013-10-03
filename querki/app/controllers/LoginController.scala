package controllers

import scala.util._

import play.api.data._
import play.api.data.Forms._
import play.api.data.format.Formats._
import play.api.data.validation.Constraints._
import play.api.mvc._

import models._

import querki.identity._
import querki.util._
import querki.values.QLRequestContext

object LoginController extends ApplicationBase {
  
  case class UserForm(name:String, password:String)
  val userForm = Form(
    mapping(
      "name" -> nonEmptyText,
      "password" -> nonEmptyText
    )((name, password) => UserForm(name, password))
     ((user: UserForm) => Some((user.name, "")))
  )
  
  val inviteForm = Form(
    mapping(
      "invitees" -> list(email)
    )((invitees) => invitees)
     ((invitees:List[String]) => Some(invitees))
  )
  
  // TODO: SignupInfo and its related form are actually written according to the Play
  // examples, now that I understand what this is all supposed to look like. The other
  // input forms should be rewritten similarly as we have time.
  val signupForm = Form(
    mapping(
      "email" -> email,
      "password" -> (nonEmptyText verifying minLength(6)),
      "handle" -> (text verifying pattern("""[a-zA-Z0-9]+""".r, error="A handle can only contain letters and numbers")),
      "display" -> nonEmptyText
    )(SignupInfo.apply)(SignupInfo.unapply)
  )
  
  def inviteMembers(ownerId:String, spaceId:String) = withSpace(true, ownerId, spaceId) { implicit rc =>
    implicit val request = rc.request
    val rawForm = inviteForm.bindFromRequest
    rawForm.fold(
      errorForm => {
        // NOTE: the theoretically-correct error message would be retrived this way, but it's unhelpful:
        //Logger.info("----> errors: " + errorForm.errors.map(error => play.api.i18n.Messages(error.message, error.args: _*)));
        // TODO: internationalize this message:
        val errorMsg = "Not a valid email address: " + errorForm.errors.flatMap(error => errorForm(error.key).value).mkString(", ")
        // TODO: this ought to reuse errorForm, to leave the invitees filled-in, but I'm not yet clear on how to do that:
        doError(routes.Application.sharing(ownerId, spaceId), errorMsg) 
      },
      emailStrs => {
        val context = QLRequestContext(rc)
        
        val invitees = emailStrs.map(modules.email.EmailAddress(_))
        val result = modules.Modules.Person.inviteMembers(rc, invitees)
        
        val resultMsg = (
          if (result.invited.length > 0)
            "Sent invitations to " + result.invited.map(_.addr).mkString(", ") + ". "
          else
            ""
        ) + (
          if (result.alreadyInvited.length > 0)
            "Already invited: " + result.alreadyInvited.map(_.addr).mkString(", ")
          else
            ""
        )
        
        Redirect(routes.Application.sharing(ownerId, spaceId)).flashing("info" -> resultMsg)
      }
    )
  }
  
  def handleInvite(ownerId:String, spaceId:String) = withSpace(false, ownerId, spaceId) { implicit rc =>
    // This cookie gets set in PersonModule.InviteLoginChecker. If it isn't set, somebody is trying to sneak
    // in through the back door:
    val emailOpt = rc.sessionCookie(modules.Modules.Person.identityEmail)
    emailOpt match {
      case Some(email) => {
        // Okay, it's a legitimate invitation. Is this a signed-in user?
        rc.requester match {
          case Some(user) => {
            // Yes. Am I already a member of this Space?
            if (querki.access.AccessControl.isMember(user, rc.state.get)) {
              Redirect(routes.Application.thing(ownerId, spaceId, spaceId))
            } else {
              // Not yet. Okay, go to joining the space:
              Ok(views.html.joinSpace(rc))
            }
          }
          case None => {
            // Nope. Let them sign up for Querki. This will loop through to signup, below:
            val startForm = SignupInfo(email, "", "", "")
            Ok(views.html.handleInvite(rc, signupForm.fill(startForm)))
          }
        }
      }
      case None => doError(routes.Application.index, "For now, you can only sign up for Querki through an invitation. Try again soon.")
    }
  }
  
  def signup(ownerId:String, spaceId:String) = withSpace(false, ownerId, spaceId) { implicit rc =>
    implicit val request = rc.request
    val rawForm = signupForm.bindFromRequest
    rawForm.fold(
      errorForm => {
        BadRequest(views.html.handleInvite(rc, errorForm))
      },
      info => {
        // Make sure we have a Person in a Space in the cookies -- that is required for a legitimate request
    	val personOpt = rc.sessionCookie(modules.Modules.Person.personParam)
    	personOpt match {
    	  case Some(personId) => {
	    	val result = User.createProvisional(info)
	        result match {
	          case Success(user) => {
	            // TODO: go to join space!
	            // We're now logged in, so start a new session. But preserve the personParam for the next step:
	            Redirect(routes.LoginController.joinSpace(ownerId, spaceId)).withSession(user.toSession :+ (modules.Modules.Person.personParam -> personId):_*)
	          }
	          case Failure(error) => {
	            val msg = error match {
	              case err:PublicException => err.display(request)
	              case _ => QLog.error("Internal Error during signup", error); "Something went wrong; please try again"
	            }
	            BadRequest(views.html.handleInvite(rc.withError(msg), rawForm))
	          }
	        }    	    
    	  }
    	  case None => doError(routes.Application.index, "For now, you can only sign up for Querki through an invitation. Try again soon.")
    	}
      }
    )
  }
  
  def joinSpace(ownerId:String, spaceId:String) = withSpace(true, ownerId, spaceId) { implicit rc =>
    val joinOpt = modules.Modules.Person.acceptInvitation(rc) {
      case ThingFound(id, state) => Redirect(routes.Application.thing(ownerId, spaceId, spaceId))
      case ThingFailed(error, msg, stateOpt) => doError(routes.Application.index, "Something went wrong adding you to the Space -- sorry!")
    }
    joinOpt match {
      case Some(f) => Async { f }
      case None => doError(routes.Application.index, "Something went wrong during joining -- sorry!")
    }
  }
  
  // TODO: that onUnauthorized will infinite-loop if it's ever invoked. What should we do instead?
  def login = 
    Security.Authenticated(forceUser, onUnauthorized) { user =>
      Action { implicit request =>
        if (user == User.Anonymous)
          Ok(views.html.login(RequestContext(request, None, UnknownOID, None, None)))
        else
          Redirect(routes.Application.index) 
      }
    }
  
  def dologin = Action { implicit request =>
    val rc = RequestContext(request, None, UnknownOID, None, None)
    userForm.bindFromRequest.fold(
      errors => doError(routes.LoginController.login, "I didn't understand that"),
      form => {
        val userOpt = User.checkQuerkiLogin(form.name, form.password)
        userOpt match {
          case Some(user) => Redirect(routes.Application.index).withSession(user.toSession:_*)
          case None => doError(routes.LoginController.login, "I don't know who you are")
        }
      }
    )
  }
  
  def logout = Action {
    Redirect(routes.Application.index).withNewSession
  }
}