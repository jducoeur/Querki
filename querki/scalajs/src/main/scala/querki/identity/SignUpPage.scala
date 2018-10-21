package querki.identity

import scala.util.Success

import org.scalajs.dom
import scalatags.JsDom.all._
import rx._
import upickle.default._
import autowire._

import org.querki.squery._
import org.querki.jquery._
import org.querki.gadgets._
import org.querki.gadgets.core.GadgetElementRef

import querki.api._
import querki.comm._
import querki.data.UserInfo
import querki.display.{ButtonGadget, QText}
import querki.display.rx._
import QuerkiEmptyable._
import querki.ecology._
import querki.globals._
import querki.pages._
import querki.session.UserFunctions
import querki.util.InputUtils

import CommonFunctions._
import UserFunctions._

/**
 * @author jducoeur
 */
class SignUpPage[T](includeSignin: Boolean)(onReady:Option[UserInfo => T])(implicit val ecology:Ecology) extends Page("signup") {
  
  lazy val Client = interface[querki.client.Client]
  lazy val StatusLine = interface[querki.display.StatusLine]
  lazy val UserAccess = interface[UserAccess]
  
  if (UserAccess.isActualUser)
    // Already logged in, so this page isn't going to work right:
    PageManager.showIndexPage()
    
  // Encapsulates some of the login guts, if we are showing that, and glue the pieces together.
  // TODO: this is awkward. Why aren't we exposing a plain old Future in the first place?
  lazy val logic = new LoginLogic()
  onReady.map { cb =>
    logic.loginPromise.future.map { _ =>
      cb(UserAccess.user.get)
    }
  }
    
  // This is a *very* primitive email-checker, but enough to start with:
  lazy val emailRegex = ".+@.+\\..+"
  
  lazy val tosAgreed = Var[Boolean](false) 
    
  lazy val emailInput = GadgetRef[RxInput]
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
  lazy val emailErrorDisplay = inUseErrorRef("That email address is already in use.")
  lazy val passwordInput = GadgetRef[RxInput]
  lazy val handleInput = GadgetRef[RxInput]
  lazy val handleErrorDisplay = inUseErrorRef("That handle is already in use. Please try another.")
  lazy val displayInput = GadgetRef[RxInput]
  lazy val signupButton = GadgetRef[RunButton]
  lazy val errorDisplay = GadgetRef.of[dom.html.Div]
  
  lazy val emailOkay = emailInput.flatMapRxOrElse(_.text)(_.matches(emailRegex), false)
  lazy val passwordOkay = passwordInput.flatMapRxOrElse(_.length)(_ >= 8, false)
  lazy val handleOkay = handleInput.flatMapRxOrElse(_.length)(_ >= 4, false)
  lazy val displayOkay = Rx {
    val displayEmpty = displayInput.rxEmpty
    !displayEmpty() 
  }
  
  lazy val loginHandleOkay = logic.handleInput.flatMapRxOrElse(_.length)(_ >= 3, false)
  lazy val loginPasswordOkay = logic.passwordInput.flatMapRxOrElse(_.length)(_ >= 8, false)
  lazy val loginOkay = Rx { loginHandleOkay() && loginPasswordOkay() }
  
  // The Sign Up button is disabled until all fields are fully filled-in.
  lazy val signupEnabled = Rx { 
    emailOkay() &&
    passwordOkay() &&
    handleOkay() &&
    displayOkay() &&
    tosAgreed()
  }
  
  def showInput(
    ref:GadgetRef[RxInput], 
    filter:Option[(JQueryEventObject, String) => Boolean], 
    lbl:String, 
    iid:String, 
    inputType:String, 
    place:String, 
    help:String, 
    inputOkay:Rx[Boolean],
    errorDisplay: Option[GadgetElementRef[dom.html.Div]] = None) = 
  {
    val goodCls = Rx { if (inputOkay()) "_signupGood" else "" }
    val checkCls = Rx { if (inputOkay()) "fa fa-check-square-o" else "fa fa-square-o" }
    
    errorDisplay.map { errGadget =>
      errGadget.whenRendered { err =>
        ref.whenRendered { inp =>
          $(inp.elem).keypress { evt: JQueryEventObject =>
            $(err.elem).hide()
          }
        }
      }
    }
    
    div(cls:="form-group",
      label(`for` := iid, lbl),
      ref <= new RxInput(filter, inputType, cls:="form-control", id := iid, placeholder := place),
      errorDisplay.map { g =>
        g
      },
      p(cls:="help-block", span(cls := goodCls, i(cls := checkCls), " ", help))
    )
  }
  
  def inUseErrorRef(msg: String): GadgetElementRef[dom.html.Div] = {
    val g = GadgetRef.of[dom.html.Div]
    g <= 
      div(cls := "alert alert-danger alert-dismissable",
        style := "display: none",
        msg,
        br(),
        a(cls := "alert-link",
          href := controllers.LoginController.sendPasswordReset().url,
          "Click here if you have forgotten your password."))
    g
  }
  
  def showTOSPanel(tosInfo: TOSInfo) = {
    val TOSInfo(version, text) = tosInfo
    
    div(
      div(cls := "panel panel-default",
        div(cls := "panel-heading", span(cls := "_signupTosHeading", s"Querki Terms of Service, version $version")),
        div(cls := "panel-body",
          div(cls:="well _signupTosBody",
            new QText(text)
          )
        )
      ),
      
      p(new RxCheckbox(tosAgreed, " I have read, and agree to, the Querki Terms of Service."))
    )
  }
  
  def doSignup():Future[UserInfo] = {
    // We call this one as a raw AJAX call, instead of going through client, since it is a weird case:
    val fut:Future[String] = 
      controllers.LoginController.signupStart().callAjax(
        "email" -> emailInput.get.text.now.trim, 
        "password" -> passwordInput.get.text.now,
        "handle" -> handleInput.get.text.now,
        "display" -> displayInput.get.text.now)
        
    fut.map { str =>
      read[UserInfo](str)
    }
  }
  
  def signup(tosInfo: TOSInfo) = {
    doSignup().flatMap { user =>
      UserAccess.setUser(Some(user))
      // Okay, we have a User -- now, check off the Terms of Service, which they had to agree to
      // in order to get to this point:
      Client[UserFunctions].agreeToTOS(tosInfo.version).call().map { _ =>
        // Iff an onReady was passed in, we're part of a workflow, so invoke that. Otherwise, we're
        // running imperatively, so just show the Index: 
        onReady.map(_(user)).getOrElse(PageManager.showIndexPage())        
      }
    }.onFailure { case th =>
      try {
        Client.translateServerException(th)
      } catch {
        case HandleAlreadyTakenException(_) => handleErrorDisplay.mapElemNow($(_).show)
        case EmailAlreadyTakenException(_) => emailErrorDisplay.mapElemNow($(_).show)
        case PlayAjaxException(jqXHR, textStatus, thrown) => {
          errorDisplay <= div(cls:="_loginError", jqXHR.responseText)
        }
        case _ : Throwable =>
      }
      // Something went wrong, so re-enable the button:
      signupButton.get.done()
    }
  }
  
  def pageContent = for {
    tos <- Client[CommonFunctions].fetchTOS().call()
    guts = div(
      if (includeSignin) {
        div(
          h1("Sign in"),
          // TODO: the following is currently hard-coded to the invite-link logic, and probably
          // shouldn't be:
          p(b(DataAccess.space.get.displayName), s" requires that you log in (if you have an account already), or sign up below."),
          
          form(
            showInput(logic.handleInput, None, "Handle or Email Address", "emailSigninInput", "text", "joe@example.com", 
                "Must be your handle or email address", loginHandleOkay, None),
            showInput(logic.passwordInput, None, "Password", "passwordSigninInput", "password", "", 
                "Must be your valid password", loginPasswordOkay, None),
            logic.badLoginMsg <= 
              div(
                cls := "alert alert-danger alert-dismissable",
                style := "display: none",
                button(
                  tpe := "button", 
                  cls := "close", 
                  data("dismiss") := "alert", 
                  aria.label := "Close", 
                  span(aria.hidden := "true", raw("&times;")),
                  tabindex := 7),
                b("That isn't a correct email and password."), 
                " Please try again. "
              ),
              
            div(cls := "row",
              div(cls := "col-sm-3",
                new ButtonGadget(
                  ButtonGadget.Primary, 
                  "Log in", 
                  disabled := Rx { !loginOkay() },
                  tabindex := 3
                )({ () => 
                  // TODO: the way doLogin works, with the Future managed as a side-effect, is hideous. Clean
                  // this up!
                  val doneFuture = logic.loginPromise.future
                  doneFuture.onComplete {
                    case _ => {
                      onReady.map(_(UserAccess.user.get)).getOrElse(PageManager.showIndexPage())
                    }
                  }
                  logic.doLogin()
                })
              )
            )
          ),
          
          p(a(href := controllers.LoginController.sendPasswordReset().url,
            "Click here if you have forgotten your password.",
            tabindex := 5)),
            
          hr
        )
      },
        
      h1(pageTitle),
      p("Please fill in all of these fields, and then press the Sign Up button to join."),
      errorDisplay <= div(),
      
      form(
        showInput(emailInput, None, "Email Address", "emailInput", "text", "joe@example.com", "Must be a valid email address", 
          emailOkay, Some(emailErrorDisplay)),
        showInput(passwordInput, None, "Password", "passwordInput", "password", "Password", "At least 8 characters", passwordOkay),
        showInput(handleInput, Some(InputUtils.nameFilter(false, false)), "Choose a Querki handle", "handleInput", "text", "Handle",
          """At least four letters and numbers, without spaces. This will be your unique id in Querki,
            |and will be used in the URLs of your Spaces. This id is permanent.""".stripMargin,
          handleOkay, Some(handleErrorDisplay)),
        showInput(displayInput, None, "Choose a Display Name", "displayInput", "text", "Name",
          """Your public name in Querki, which will show most of the time. This may be your real-life name,
            |but does not have to be. You can change this later.""".stripMargin,
          displayOkay),
          
        showTOSPanel(tos),

        signupButton <= new RunButton(ButtonGadget.Primary, "Sign Up", "Signing up...", id := "signupButton", disabled := Rx { !signupEnabled() }) 
          ({ _ => signup(tos) })
      )
    )
  }
    yield PageContents(guts)
}

object SignUpPage {
  /**
   * Encapsulates the SignUp workflow so that other systems can compose it.
   */
  def run(includeSignin: Boolean)(implicit ecology:Ecology):Future[UserInfo] = {
    val promise = Promise[UserInfo]
    val completeFunc:UserInfo => Unit = user => promise.complete(Success(user))
    val page = new SignUpPage(includeSignin)(Some(completeFunc))
    // NOTE: this doesn't actually change the URL! This is arguably a horrible hack.
    ecology.api[querki.display.PageManager].renderPage(page)
    promise.future
  }
}
