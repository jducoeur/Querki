package controllers

import scala.util._

import play.api.data._
import play.api.data.Forms._
import play.api.data.format.Formats._
import play.api.data.validation.Constraints._
import play.api.mvc._

import models._

import querki.ecology._
import querki.email.EmailAddress
import querki.identity._
import querki.spaces.messages.{ThingError, ThingFound}
import querki.time.DateTime
import querki.util._
import querki.values.QLRequestContext

case class PasswordChangeInfo(password:String, newPassword:String, newPasswordAgain:String)

class LoginController extends ApplicationBase {

  lazy val Email = interface[querki.email.Email]
  lazy val Encryption = interface[querki.security.Encryption]
  lazy val Person = interface[querki.identity.Person]
  
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
      "password" -> (nonEmptyText verifying minLength(8)),
      "handle" -> (text verifying pattern("""[a-zA-Z0-9]+""".r, error="A handle can only contain letters and numbers")),
      "display" -> nonEmptyText
    )(SignupInfo.apply)(SignupInfo.unapply)
  )
  
  val passwordChangeForm = Form(
    mapping(
      "password" -> nonEmptyText,
      "newPassword" -> (nonEmptyText verifying minLength(8)),
      "newPasswordAgain" -> (nonEmptyText verifying minLength(8))
    )(PasswordChangeInfo.apply)(PasswordChangeInfo.unapply)
  )
  
  val resetPasswordForm = Form(
    mapping("name" -> nonEmptyText)((handle) => handle)((handle:String) => Some(handle))
  )
  
  lazy val maxMembers = Config.getInt("querki.public.maxMembersPerSpace", 100)
  
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
        val nCurrentMembers = Person.people(rc.state.get).size
        // TODO: internationalize these messages!
        val resultMsg =
	        if (!rc.requesterOrAnon.isAdmin && (nCurrentMembers + emailStrs.size) > maxMembers) {
	          s"Sorry: at the moment you are limited to $maxMembers members per Space, and this would make more than that."
	        } else {
	          val context = QLRequestContext(rc)
	        
	          val invitees = emailStrs.map(querki.email.EmailAddress(_))
	          val result = Person.inviteMembers(rc, invitees)
	        
	          (
	            if (result.invited.length > 0)
	               result.invited.map(_.addr).mkString("Sent invitations to ", ", ", ". ")
	            else
	              ""
	          ) + (
	            if (result.alreadyInvited.length > 0)
	               result.alreadyInvited.map(_.addr).mkString("Resent to ", ", ", ".") 
	            else
	              ""
	          )
	        }
        
        Redirect(routes.Application.sharing(ownerId, spaceId)).flashing("info" -> resultMsg)
      }
    )
  }
  
  def handleInvite(ownerId:String, spaceId:String) = withSpace(false, ownerId, spaceId, allowAnyone = true) { implicit rc =>
    // This cookie gets set in PersonModule.InviteLoginChecker. If it isn't set, somebody is trying to sneak
    // in through the back door:
    val emailOpt = rc.sessionCookie(querki.identity.identityEmail)
    emailOpt match {
      case Some(email) => {
        // Okay, it's a legitimate invitation. Is this a signed-in user?
        rc.requester match {
          case Some(user) => {
            // Yes. Am I already a member of this Space?
            if (AccessControl.isMember(user, rc.state.get)) {
              // Yes. Okay, just go the Space, since there's nothing to do here:
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
  
  // TODO: can we factor this together with dologin in a sensible way? The trick is that we want to finish login by showing
  // joinSpace...
  def joinlogin(ownerId:String, spaceId:String) = withSpace(false, ownerId, spaceId, allowAnyone = true) { implicit rc =>
    implicit val request = rc.request
    userForm.bindFromRequest.fold(
      errors => doError(Call(request.method, request.path), "I didn't understand that"),
      form => {
        val userOpt = UserAccess.checkQuerkiLogin(form.name, form.password)
        userOpt match {
          case Some(user) => {
            // Yes. Am I already a member of this Space?
            if (AccessControl.isMember(user, rc.state.get)) {
              // Yes. Okay, just go the Space, since there's nothing to do here:
              Redirect(routes.Application.thing(ownerId, spaceId, spaceId)).withSession(user.toSession:_*)
            } else {
              Ok(views.html.joinSpace(rc)).withSession(Session(request.session.data ++ user.toSession))
            }
          }
          case None => doError(Call(request.method, request.path), "Login failed. Please try again.")
        }
      }
    )
  }
    
  def signup(ownerId:String, spaceId:String) = withSpace(false, ownerId, spaceId, allowAnyone = true) { implicit rc =>
    implicit val request = rc.request
    val rawForm = signupForm.bindFromRequest
    rawForm.fold(
      errorForm => {
        BadRequest(views.html.handleInvite(rc, errorForm))
      },
      info => {
        // Make sure we have a Person in a Space in the cookies -- that is required for a legitimate request
    	val personOpt = rc.sessionCookie(querki.identity.personParam)
    	personOpt match {
    	  case Some(personId) => {
	    	val result = UserAccess.createProvisional(info)
	        result match {
	          case Success(user) => {
	            // We're now logged in, so start a new session. But preserve the personParam for the next step:
	            Redirect(routes.LoginController.joinSpace(ownerId, spaceId)).withSession(user.toSession :+ (querki.identity.personParam -> personId):_*)
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
  
  def joinSpace(ownerId:String, spaceId:String) = withSpace(true, ownerId, spaceId, allowAnyone = true) { implicit rc =>
    val joinOpt = Person.acceptInvitation(rc) {
      case ThingFound(id, state) => Redirect(routes.Application.thing(ownerId, spaceId, spaceId))
      case ThingError(error, stateOpt) => doError(routes.Application.index, error)
    }
    joinOpt match {
      case Some(f) => Async { f }
      case None => doError(routes.Application.index, "Something went wrong during joining -- sorry!")
    }
  }

  def userByName(userName:String) = withUser(true) { rc =>
    val pairOpt = UserAccess.getIdentity(ThingId(userName))
    pairOpt match {
      case Some((identity, level)) => {
        val initialPasswordForm = PasswordChangeInfo("", "", "")
        Ok(views.html.profile(rc, identity, level, passwordChangeForm.fill(initialPasswordForm)))
      }
      case None => doError(routes.Application.index, "That isn't a legal path")
    }
  }
  
  def changePassword(handle:String) = withUser(true) { rc =>
    implicit val request = rc.request
    val rawForm = passwordChangeForm.bindFromRequest
    rawForm.fold(
      errorForm => doError(routes.LoginController.userByName(handle), "That was not a legal password"),
      info => {
        val checkedLogin = UserAccess.checkQuerkiLogin(handle, info.password)
        checkedLogin match {
          case Some(user) => {
            if (info.newPassword == info.newPasswordAgain) {
              val identity = user.identityByHandle(handle).get
              val newUser = UserAccess.changePassword(rc.requesterOrAnon, identity, info.newPassword)
              Redirect(routes.LoginController.userByName(handle)).flashing("info" -> "Password changed")
            } else {
              doError(routes.LoginController.userByName(handle), "The passwords didn't match")
            }
          }
          case _ => {
            doError(routes.LoginController.userByName(handle), "The current password was incorrect. Please try again.")
          }
        }
      }
    )
  }
  
  def sendPasswordReset() = withUser(false) { rc =>
    Ok(views.html.sendPasswordReset(rc))
  }
  
  def resetValidationStr(email:String, expires:Long) = s"${email} ${expires}"
  
  def doSendPasswordReset() = withUser(false) { rc =>
    implicit val request = rc.request
    val rawForm = resetPasswordForm.bindFromRequest
    // We show the same error in all cases, to avoid information leakage
    def showError = {
      doError(routes.LoginController.sendPasswordReset, "That isn't a known login handle or email address")
    }
    rawForm.fold(
      errorForm => showError,
      handle => {
        val successOpt = for {
          user <- UserAccess.getUserByHandleOrEmail(handle)
          identity <- user.loginIdentity
          email = identity.email.addr
          expires = DateTime.now.plusDays(2).getMillis()
          hash = Encryption.calcHash(resetValidationStr(email, expires))
          subject = Wikitext("Reset your Querki password")
          body = Wikitext(s"""We received a request to reset the password for your account, ${user.mainIdentity.handle}.
            |If you made this request, please [click here](${routes.LoginController.resetPassword(email, expires, hash).absoluteURL(false)(rc.request)}), which will take you to a page
            |where you can enter a new password for your Querki account. This link will only be valid for the next two days, so please act
            |on it soon!
            |
            |If you did not make this request, please just ignore this email, and nothing will be changed.""".stripMargin)
          result = Email.sendSystemEmail(identity, subject, body)
        }
          yield true
          
        successOpt match {
          case Some(true) => doInfo(routes.Application.index, "Password update email has been sent")
          case _ => showError
        }
      }
    )
  }
  
  def resetPassword(email:String, expiresMillis:Long, hash:String) = withUser(false) { rc =>
    val initialPasswordForm = PasswordChangeInfo(hash, "", "")
    Ok(views.html.resetPassword(rc, email, expiresMillis, hash, passwordChangeForm.fill(initialPasswordForm)))
  }
  
  def doResetPassword(email:String, expiresMillis:Long, hash:String) = withUser(false) { rc =>
    def showError() = doError(routes.LoginController.resetPassword(email, expiresMillis, hash), "Invalid password change")
    if (!Encryption.authenticate(resetValidationStr(email, expiresMillis), hash))
      showError()
    else {
      val expires = new DateTime(expiresMillis)
      if (expires.isBeforeNow())
        showError()
      else {
        implicit val request = rc.request
        val rawForm = passwordChangeForm.bindFromRequest
        rawForm.fold(
          errorForm => showError(),
          info => {
            if (info.newPassword == info.newPasswordAgain) {
              UserAccess.getUserByHandleOrEmail(email) match {
                case Some(user) => {
                  val identity = user.loginIdentity.get
                  val newUser = UserAccess.changePassword(user, identity, info.newPassword)
                  Redirect(routes.Application.index).flashing("info" -> "Password changed")
                }
                case None => showError()
              }              
            } else {
              doError(routes.LoginController.resetPassword(email, expiresMillis, hash), "The passwords didn't match")
            }
          }
        )
      }
    }
  }
  
  // login now simply happens through the index page
  def login = Redirect(routes.Application.index)
  
  def dologin = Action { implicit request =>
    val rc = PlayRequestContext(request, None, UnknownOID, None, None, ecology)
    userForm.bindFromRequest.fold(
      errors => doError(routes.Application.index, "I didn't understand that"),
      form => {
        val userOpt = UserAccess.checkQuerkiLogin(form.name, form.password)
        userOpt match {
          case Some(user) => {
            val redirectOpt = rc.sessionCookie(rc.returnToParam)
		    redirectOpt match {
		      case Some(redirect) => Redirect(redirect).withSession(user.toSession:_*)
		      case None => Redirect(routes.Application.index).withSession(user.toSession:_*)
		    }
          }
          case None => doError(routes.Application.index, "Login failed. Please try again.")
        }
      }
    )
  }
  
  def logout = Action {
    Redirect(routes.Application.index).withNewSession
  }
}