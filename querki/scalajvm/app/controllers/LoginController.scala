package controllers

import scala.concurrent.ExecutionContext.Implicits.global 
import scala.concurrent.Future
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
import querki.spaces.messages._
import querki.time.DateTime
import querki.util._
import querki.values.QLRequestContext

case class PasswordChangeInfo(password:String, newPassword:String, newPasswordAgain:String)

class LoginController extends ApplicationBase {

  lazy val Email = interface[querki.email.Email]
  lazy val Encryption = interface[querki.security.Encryption]
  lazy val Person = interface[querki.identity.Person]
  lazy val UserSession = interface[querki.session.Session]
  
  case class UserForm(name:String, password:String)
  val userForm = Form(
    mapping(
      "name" -> nonEmptyText,
      "password" -> nonEmptyText
    )((name, password) => UserForm(name, password))
     ((user: UserForm) => Some((user.name, "")))
  )
  
  case class InviteeForm(emails:List[String], collabs:List[String])
  val inviteForm = Form(
    mapping(
      "invitees" -> list(email),
      "collaborators" -> list(nonEmptyText)
    )((emails, collaborators) => InviteeForm(emails, collaborators))
     ((invitees:InviteeForm) => Some((invitees.emails, invitees.collabs)))
  )
  
  // TODO: SignupInfo and its related form are actually written according to the Play
  // examples, now that I understand what this is all supposed to look like. The other
  // input forms should be rewritten similarly as we have time.
  val signupForm = Form(
    mapping(
      "email" -> email,
      // Note that we intentionally do *not* validate the minimum length here, because Play
      // foolishly display the error with the password in plaintext on the screen:
      "password" -> nonEmptyText,
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
  
  val displayNameChangeForm = Form(
    mapping("newName" -> nonEmptyText)((name) => name)((name:String) => Some(name))
  )
  
  val resetPasswordForm = Form(
    mapping("name" -> nonEmptyText)((handle) => handle)((handle:String) => Some(handle))
  )
  
  def getCollaborators(ownerId:String, spaceId:String, q:String) = withSpace(true, ownerId, spaceId) { implicit rc =>
    UserSession.getCollaborators(rc.requesterOrAnon, rc.localIdentity.get, q).map { collabs =>
      // TODO: introduce better JSONification for the AJAX code:
      // TODO: refactor this with getTags and getLinks; there is a common "return Manifest" function here:
      val JSONcollabs = "[" + collabs.acs.map(identity => "{\"display\":\"" + identity.name + "\", \"id\":\"" + identity.id.toThingId + "\"}").mkString(",") + "]"
      Ok(JSONcollabs)
    }
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
              Redirect(routes.ClientController.space(ownerId, spaceId))
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
              Redirect(routes.ClientController.space(ownerId, spaceId)).withSession(user.toSession:_*)
            } else {
              Ok(views.html.joinSpace(rc)).withSession(Session(request.session.data ++ user.toSession))
            }
          }
          case None => doError(Call(request.method, request.path), "Login failed. Please try again.")
        }
      }
    )
  }
  
  val minPasswordLen = 8
  
  def passwordValidationError(password:String):Option[String] = {
    // For now, we're not doing much except the most trivial check:
    if (password.length >= minPasswordLen)
      None
    else
      Some(s"Password must be at least $minPasswordLen characters long")
  }
    
  // Is there any reason this actually needs withSpace, other than proving the Space exists?
  def signup(ownerId:String, spaceId:String) = withSpace(false, ownerId, spaceId, allowAnyone = true) { implicit rc =>
    implicit val request = rc.request
    val rawForm = signupForm.bindFromRequest
    rawForm.fold(
      errorForm => {
        BadRequest(views.html.handleInvite(rc, errorForm))
      },
      info => {
        passwordValidationError(info.password) match {
          case Some(err) => BadRequest(views.html.handleInvite(rc.withError(err), rawForm))
          case None => {
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
        }
      }
    )
  }
  
  def joinSpace(ownerId:String, spaceId:String) = withRouting(ownerId, spaceId) { rc =>
    askSpace(rc.ownerId, rc.spaceIdOpt.get)(SpaceMembersMessage(rc.requesterOrAnon, _, JoinRequest(rc))) {
      case Joined => Redirect(routes.ClientController.space(ownerId, spaceId))
      case JoinFailed(error) => doError(routes.Application.index, error)(rc)
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
  
  def changeDisplayName(identityIdStr:String) = withUser(true) { rc =>
    implicit val request = rc.request
    val rawForm = displayNameChangeForm.bindFromRequest
    // We show the same error in all cases, to avoid information leakage
    def showError = {
      doError(routes.LoginController.userByName(rc.requesterOrAnon.mainIdentity.handle), "That isn't a legal Display Name")
    }
    
    val identityId = OID(identityIdStr)
    rc.requesterOrAnon.identityById(identityId) match {
      case None => showError
      case Some(identity) => {
	    rawForm.fold(
	      errorForm => showError,
	      name => {
	        val newUser = UserAccess.changeDisplayName(rc.requesterOrAnon, identity:Identity, name)
	        Redirect(routes.LoginController.userByName(identity.handle)).
	          flashing("info" -> s"Display Name changed to $name. This change will show up in your Spaces the next time you use them.")
	      }
	    )
      }
    }
  }
  
  // login now simply happens through the index page
  def login = Redirect(routes.Application.index)
  
  def dologin = Action.async { implicit request =>
    val rc = PlayRequestContextFull(request, None, UnknownOID, None, None, ecology)
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