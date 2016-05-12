package querki.identity

import scalatags.JsDom.all._
import rx._

import querki.display.rx._
import querki.ecology._
import querki.globals._
import querki.pages._

/**
 * @author jducoeur
 */
class SignUpPage(implicit e:Ecology) extends Page(e, "signup") {
  
  lazy val UserAccess = interface[UserAccess]
  
  if (UserAccess.user.isDefined)
    // Already logged in, so this page isn't going to work right:
    PageManager.showIndexPage()
    
  lazy val emailInput = GadgetRef[RxInput]
  lazy val passwordInput = GadgetRef[RxInput]
  lazy val handleInput = GadgetRef[RxInput]
  lazy val displayInput = GadgetRef[RxInput]
  
  // The Sign Up button is disabled until all fields are fully filled-in.
  // TODO: more validation! Validate the format of the email, and minimum length on handle and display.
  lazy val signupDisabled = Rx { 
    emailInput.isContentEmpty() ||
    passwordInput.map(_.length < 8).getOrElse(true) ||
    handleInput.isContentEmpty() ||
    displayInput.isContentEmpty()
  }
  
  def showInput(ref:GadgetRef[RxInput], lbl:String, iid:String, inputType:String, place:String, help:Option[String] = None) = {
    div(cls:="form-group",
      label(`for` := iid, lbl),
      ref <= new RxInput(inputType, cls:="form-control", id := iid, placeholder := place),
      help.map(h => p(cls:="help-block", h))
    )
  }
  
  def pageContent = for {
    guts <- scala.concurrent.Future.successful(div(
      h1(pageTitle),
      p("Please fill in all of these fields, and then press the Sign Up button to join."),
      
      form(
        showInput(emailInput, "Email Address", "emailInput", "text", "joe@example.com"),
        showInput(passwordInput, "Password", "passwordInput", "password", "Password", Some("At least 8 characters")),
        showInput(handleInput, "Choose a Querki handle", "handleInput", "text", "Handle",
          Some("""Letters and numbers only, without spaces. (Basic ASCII only.) This will be your unique id in Querki,
                 |and will be used in the URLs of your Spaces. This id is permanent.""".stripMargin)),
        showInput(displayInput, "Choose a Display Name", "displayInput", "text", "Name",
          Some("""Your public name in Querki, which will show most of the time. This may be your real-life name,
                 |but does not have to be. You can change this later.""".stripMargin)),
                 
        button(tpe:="submit", cls:="btn btn-primary", disabled := signupDisabled, "Sign Up")
      )
    ))
  }
    yield PageContents(guts)
}
