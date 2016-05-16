package querki.identity

import org.scalajs.dom
import scalatags.JsDom.all._
import rx._
import upickle._

import querki.api._
import querki.comm._
import querki.data.UserInfo
import querki.display.ButtonGadget
import querki.display.rx._
import querki.ecology._
import querki.globals._
import querki.pages._

/**
 * @author jducoeur
 */
class SignUpPage(implicit e:Ecology) extends Page(e, "signup") {
  
  lazy val StatusLine = interface[querki.display.StatusLine]
  lazy val UserAccess = interface[UserAccess]
  
  if (UserAccess.user.isDefined)
    // Already logged in, so this page isn't going to work right:
    PageManager.showIndexPage()
    
  lazy val emailInput = GadgetRef[RxInput]
  lazy val passwordInput = GadgetRef[RxInput]
  lazy val handleInput = GadgetRef[RxInput]
  lazy val displayInput = GadgetRef[RxInput]
  lazy val signupButton = GadgetRef[RunButton]
  lazy val errorDisplay = GadgetRef.of[dom.html.Div]
  
  // The Sign Up button is disabled until all fields are fully filled-in.
  // TODO: more validation! Validate the format of the email and handle. Even more
  // than that, each field should display a checkmark or something saying that it is properly filled in.
  lazy val signupDisabled = Rx { 
    emailInput.isContentEmpty() ||
    passwordInput.mapOrElse(_.length < 8, true) ||
    handleInput.mapOrElse(_.length < 4, true) ||
    displayInput.isContentEmpty()
  }
  
  def showInput(ref:GadgetRef[RxInput], lbl:String, iid:String, inputType:String, place:String, help:Option[String] = None) = {
    div(cls:="form-group",
      label(`for` := iid, lbl),
      ref <= new RxInput(inputType, cls:="form-control", id := iid, placeholder := place),
      help.map(h => p(cls:="help-block", h))
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
        showInput(emailInput, "Email Address", "emailInput", "text", "joe@example.com"),
        showInput(passwordInput, "Password", "passwordInput", "password", "Password", Some("At least 8 characters")),
        showInput(handleInput, "Choose a Querki handle", "handleInput", "text", "Handle",
          Some("""At least four letters and numbers, without spaces. (Basic ASCII only.) This will be your unique id in Querki,
                 |and will be used in the URLs of your Spaces. This id is permanent.""".stripMargin)),
        showInput(displayInput, "Choose a Display Name", "displayInput", "text", "Name",
          Some("""Your public name in Querki, which will show most of the time. This may be your real-life name,
                 |but does not have to be. You can change this later.""".stripMargin)),
                 
        signupButton <= new RunButton(ButtonGadget.Primary, "Sign Up", "Signing up...", disabled := signupDisabled) 
          ({ _ => signup() })
      )
    ))
  }
    yield PageContents(guts)
}
