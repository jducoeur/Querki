package querki.identity

import scala.util.Success

import org.scalajs.dom
import scalatags.JsDom.all._
import rx._
import upickle.default._

import org.querki.squery._
import org.querki.jquery._

import querki.api._
import querki.comm._
import querki.data.UserInfo
import querki.display.ButtonGadget
import querki.display.rx._
import RxEmptyable._
import querki.ecology._
import querki.globals._
import querki.pages._
import querki.util.InputUtils

/**
 * @author jducoeur
 */
class SignUpPage[T](onReady:Option[UserInfo => T])(implicit val ecology:Ecology) extends Page("signup") {
  
  lazy val StatusLine = interface[querki.display.StatusLine]
  lazy val UserAccess = interface[UserAccess]
  
  if (UserAccess.isActualUser)
    // Already logged in, so this page isn't going to work right:
    PageManager.showIndexPage()
    
  // This is a *very* primitive email-checker, but enough to start with:
  lazy val emailRegex = ".+@.+\\..+"
    
  lazy val emailInput = QGadgetRef[RxInput]
    .whenRendered { g =>
      for {
        // If we are currently showing a Guest (there's a User, and we've already established above
        // that it isn't an actual User), and there's an email cookie (set in handleInvite2()), then
        // use that as the default.
        user <- UserAccess.user
        guestEmail <- Cookies.get("guestEmail")
      }
        g.setValue(guestEmail)
    }
  lazy val passwordInput = QGadgetRef[RxInput]
  lazy val handleInput = QGadgetRef[RxInput]
  lazy val displayInput = QGadgetRef[RxInput]
  lazy val signupButton = QGadgetRef[RunButton]
  lazy val errorDisplay = QGadgetRef.of[dom.html.Div]
  
  lazy val emailOkay = Rx { emailInput.map(_.text().matches(emailRegex)).getOrElse(false) }
  lazy val passwordOkay = Rx { passwordInput.mapOrElse(_.length >= 8, false) }
  lazy val handleOkay = Rx { handleInput.mapOrElse(_.length >= 4, false) }
  lazy val displayOkay = Rx { !displayInput.rxEmpty() }
  
  // The Sign Up button is disabled until all fields are fully filled-in.
  lazy val signupEnabled = Rx { 
    emailOkay() &&
    passwordOkay() &&
    handleOkay() &&
    displayOkay()
  }
  
  def showInput(ref:QGadgetRef[RxInput], filter:Option[(JQueryEventObject, String) => Boolean], lbl:String, iid:String, inputType:String, place:String, help:String, inputOkay:Rx[Boolean]) = 
  {
    val goodCls = Rx { if (inputOkay()) "_signupGood" else "" }
    val checkCls = Rx { if (inputOkay()) "fa fa-check-square-o" else "fa fa-square-o" }
    
    div(cls:="form-group",
      label(`for` := iid, lbl),
      ref <= new RxInput(filter, inputType, cls:="form-control", id := iid, placeholder := place),
      p(cls:="help-block", span(cls := goodCls, i(cls := checkCls), " ", help))
    )
  }
  
  def doSignup():Future[UserInfo] = {
    // We call this one as a raw AJAX call, instead of going through client, since it is a weird case:
    val fut:Future[String] = 
      controllers.LoginController.signupStart().callAjax(
        "email" -> emailInput.get.text().trim, 
        "password" -> passwordInput.get.text(),
        "handle" -> handleInput.get.text(),
        "display" -> displayInput.get.text())
        
    fut.map { str =>
      read[UserInfo](str)
    }
  }
  
  def signup() = {
    doSignup().map { user =>
      UserAccess.setUser(Some(user))
      // Okay, we have a User -- now, deal with Terms of Service:
      TOSPage.run.map { _ =>
        // Iff an onReady was passed in, we're part of a workflow, so invoke that. Otherwise, we're
        // running imperatively, so just show the Index: 
        onReady.map(_(user)).getOrElse(PageManager.showIndexPage())        
      }
    }.onFailure { case th =>
      th match {
        case PlayAjaxException(jqXHR, textStatus, thrown) => {
          errorDisplay <= div(cls:="_loginError", jqXHR.responseText)
        }
        case _ =>
      }
      // Something went wrong, so re-enable the button:
      signupButton.get.done()
    }
  }
  
  def pageContent = for {
    guts <- scala.concurrent.Future.successful(div(
      h1(pageTitle),
      p("Please fill in all of these fields, and then press the Sign Up button to join."),
      errorDisplay <= div(),
      
      form(
        showInput(emailInput, None, "Email Address", "emailInput", "text", "joe@example.com", "Must be a valid email address", emailOkay),
        showInput(passwordInput, None, "Password", "passwordInput", "password", "Password", "At least 8 characters", passwordOkay),
        showInput(handleInput, Some(InputUtils.nameFilter(false, false)), "Choose a Querki handle", "handleInput", "text", "Handle",
          """At least four letters and numbers, without spaces. This will be your unique id in Querki,
            |and will be used in the URLs of your Spaces. This id is permanent.""".stripMargin,
          handleOkay),
        showInput(displayInput, None, "Choose a Display Name", "displayInput", "text", "Name",
          """Your public name in Querki, which will show most of the time. This may be your real-life name,
            |but does not have to be. You can change this later.""".stripMargin,
          displayOkay),

        signupButton <= new RunButton(ButtonGadget.Primary, "Sign Up", "Signing up...", id := "signupButton", disabled := Rx { !signupEnabled() }) 
          ({ _ => signup() })
      )
    ))
  }
    yield PageContents(guts)
}

object SignUpPage {
  /**
   * Encapsulates the SignUp workflow so that other systems can compose it.
   */
  def run(implicit ecology:Ecology):Future[UserInfo] = {
    val promise = Promise[UserInfo]
    val completeFunc:UserInfo => Unit = user => promise.complete(Success(user))
    val page = new SignUpPage(Some(completeFunc))
    // NOTE: this doesn't actually change the URL! This is arguably a horrible hack.
    ecology.api[querki.display.PageManager].renderPage(page)
    promise.future
  }
}
