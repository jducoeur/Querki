package querki.pages

import scala.util.{Failure, Success}
import org.scalajs.dom
import scalatags.JsDom.all._
import autowire._
import rx._

import org.querki.jquery._

import querki.api.BadPasswordException
import querki.display.ButtonGadget
import querki.display.rx._
import querki.globals._
import querki.identity.UserLevel
import querki.session.UserFunctions
import UserFunctions._

/**
 * The page that shows my account information.
 * 
 * @author jducoeur
 */
class AccountPage(params:ParamMap)(implicit e:Ecology) extends Page(e) with EcologyMember {
  lazy val Client = interface[querki.client.Client]
  lazy val StatusLine = interface[querki.display.StatusLine]

/*
    <div class="row">
      <h3>Basic Information</h3>
      <p><b>Handle:</b> @identity.handle</p>
      <p><b>Email:</b> @identity.email.addr</p>
      <p><b>Status:</b> @UserLevel.levelName(level)</p>
    
      <h3>Change Your Password</h3>
      
      <p>Enter your old password, and then the new one. Passwords must be at least 8 characters long.</p>
      
      @form(routes.LoginController.changePassword(identity.handle)) {     
        @inputPassword(
            passwordChangeForm("password"), 
            '_label -> "Your Current Password",
            '_showConstraints -> false
          )
        
          @inputPassword(
            passwordChangeForm("newPassword"), 
            '_label -> "New Password",
            '_showConstraints -> false
          )
      
          @inputPassword(
            passwordChangeForm("newPasswordAgain"), 
            '_label -> "New Password Again",
            '_showConstraints -> false
          )
          
          <input type="submit" value="Change Password" class="btn btn-primary">
      }
      
      <h3>Change Your Display Name</h3>
      
      <p>Your Display Name is currently @identity.name. Use the form below if you would like to change it.</p>
      
      @form(routes.LoginController.changeDisplayName(identity.id.toString), 'class -> "form-inline") {
        <input type="text" name="newName" id="newName" value="" placeholder="New Display Name">
        <input type="submit" value="Change Display Name" class="btn btn-primary">
      }
      
      </div>  
 */
  
  val oldPassword = GadgetRef[RxInput]
  val newPassword = GadgetRef[RxInput]
  val newPasswordRepeat = GadgetRef[RxInput]
  def passText(ref:GadgetRef[RxInput]) = Rx { ref.opt().map(input => input.text()).getOrElse("") }
  val passwordsFilled = 
    Rx { 
      passText(oldPassword)().length() > 0 &&
      passText(newPassword)().length() > 7 &&
      passText(newPassword)() == passText(newPasswordRepeat)()
    }

  def pageContent = for {
    accountInfo <- Client[UserFunctions].accountInfo().call()
    guts =
      div(
        h1("Your Account"),
        
        h3("Basic Information"),
        p(b("Handle:"), accountInfo.handle),
        p(b("Email:"), accountInfo.email),
        p(b("Status:"), UserLevel.levelName(accountInfo.level)),
        
        h3("Change Your Password"),
        p("Enter your current password, and then the new one to change it to. Passwords must be at least 8 characters long."),
        p(b("Your Current Password: "), oldPassword <= new RxInput("password")),
        p(b("New Password: "), newPassword <= new RxInput("password")),
        p(b("New Password Again: "), newPasswordRepeat <= new RxInput("password")),
        new ButtonGadget(ButtonGadget.Normal, "Change Password", disabled:= Rx { !passwordsFilled() } )({ () =>
          Client[UserFunctions].changePassword(passText(oldPassword)(), passText(newPassword)()).call().onComplete {
            case Success(dummy) => StatusLine.showBriefly("Password changed")
            case Failure(ex) =>
              ex match {
                case BadPasswordException() => StatusLine.showBriefly("You didn't give your password correctly. Please try again.")
                case _ => StatusLine.showBriefly(ex.toString())
              }
          }
        })
      )
  }
    yield PageContents("Your Account", guts)
}