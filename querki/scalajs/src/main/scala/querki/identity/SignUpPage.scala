package querki.identity

import org.scalajs.dom
import scalatags.JsDom.all._
import rx._
import upickle._
import org.querki.jquery._

import querki.api._
import querki.comm._
import querki.data.UserInfo
import querki.display.ButtonGadget
import querki.display.rx._
import querki.ecology._
import querki.globals._
import querki.pages._
import querki.util.InputUtils

/**
 * @author jducoeur
 */
class SignUpPage(implicit e:Ecology) extends Page(e, "signup") {
  
  lazy val StatusLine = interface[querki.display.StatusLine]
  lazy val UserAccess = interface[UserAccess]
  
  if (UserAccess.user.isDefined)
    // Already logged in, so this page isn't going to work right:
    PageManager.showIndexPage()
    
  // This is a *very* primitive email-checker, but enough to start with:
  lazy val emailRegex = ".+@.+\\..+"
    
  lazy val emailInput = GadgetRef[RxInput]
  lazy val passwordInput = GadgetRef[RxInput]
  lazy val handleInput = GadgetRef[RxInput]
  lazy val displayInput = GadgetRef[RxInput]
  lazy val signupButton = GadgetRef[RunButton]
  lazy val errorDisplay = GadgetRef.of[dom.html.Div]
  
  lazy val emailOkay = Rx { emailInput.map(_.text().matches(emailRegex)).getOrElse(false) }
  lazy val passwordOkay = Rx { passwordInput.mapOrElse(_.length >= 8, false) }
  lazy val handleOkay = Rx { handleInput.mapOrElse(_.length >= 4, false) }
  lazy val displayOkay = Rx { !displayInput.isContentEmpty() }
  
  // The Sign Up button is disabled until all fields are fully filled-in.
  lazy val signupEnabled = Rx { 
    emailOkay() &&
    passwordOkay() &&
    handleOkay() &&
    displayOkay()
  }
  
  def showInput(ref:GadgetRef[RxInput], filter:Option[JQueryEventObject => Boolean], lbl:String, iid:String, inputType:String, place:String, help:String, inputOkay:Rx[Boolean]) = 
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
        "email" -> emailInput.get.text(), 
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
      PageManager.showIndexPage()
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
        showInput(handleInput, Some(InputUtils.nameFilter(false)), "Choose a Querki handle", "handleInput", "text", "Handle",
          """At least four letters and numbers, without spaces. This will be your unique id in Querki,
            |and will be used in the URLs of your Spaces. This id is permanent.""".stripMargin,
          handleOkay),
        showInput(displayInput, None, "Choose a Display Name", "displayInput", "text", "Name",
          """Your public name in Querki, which will show most of the time. This may be your real-life name,
            |but does not have to be. You can change this later.""".stripMargin,
          displayOkay),

        signupButton <= new RunButton(ButtonGadget.Primary, "Sign Up", "Signing up...", disabled := Rx { !signupEnabled() }) 
          ({ _ => signup() })
      )
    ))
  }
    yield PageContents(guts)
}
