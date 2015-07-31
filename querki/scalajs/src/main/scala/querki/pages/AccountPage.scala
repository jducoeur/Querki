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
  lazy val UserAccess = interface[querki.identity.UserAccess]

  val oldPassword = GadgetRef[RxInput]
  val newPassword = GadgetRef[RxInput]
  val newPasswordRepeat = GadgetRef[RxInput]
  def passText[T <: RxInput](ref:GadgetRef[T]) = Rx { ref.opt().map(input => input.text()).getOrElse("") }
  val passwordsFilled = 
    Rx { 
      passText(oldPassword)().length() > 0 &&
      passText(newPassword)().length() > 7 &&
      passText(newPassword)() == passText(newPasswordRepeat)()
    }
  
  val newDisplayName = GadgetRef[RxText]
  
  def passwordLine(labl:String, gadget:GadgetRef[RxInput]) = 
    div(cls:="form-group",
      label(cls:="control-label col-md-2", labl), 
      div(cls:="col-md-4", gadget <= new RxInput("password", cls:="form-control")))
      
  def staticLine(labl:String, text:String) =
    div(cls:="form-group",
      label(cls:="control-label col-md-2", labl),
      div(cls:="col-md-4",
        p(cls:="form-control-static", text)
      )
    )

  def pageContent = for {
    accountInfo <- Client[UserFunctions].accountInfo().call()
    guts =
      div(
        h1("Your Account"),
        
        h3("Basic Information"),
        form(cls:="form-horizontal col-md-12",
          staticLine("Handle", accountInfo.handle),
          staticLine("Email", accountInfo.email),
          staticLine("Status", UserLevel.levelName(accountInfo.level))
        ),
        
        h3("Change Your Password"),
        p("Enter your current password, and then the new one to change it to. Passwords must be at least 8 characters long."),
        form(cls:="form-horizontal col-md-12",
          passwordLine("Your Current Password", oldPassword),
          passwordLine("New Password", newPassword),
          passwordLine("New Password Again", newPasswordRepeat),
          div(cls:="form-group",
            div(cls:="col-md-offset-2",
              new ButtonGadget(ButtonGadget.Normal, "Change Password", disabled:= Rx { !passwordsFilled() } )({ () =>
                Client[UserFunctions].changePassword(passText(oldPassword)(), passText(newPassword)()).call().onComplete {
                  case Success(dummy) => StatusLine.showBriefly("Password changed")
                  case Failure(ex) =>
                    ex match {
                      case BadPasswordException() => StatusLine.showBriefly("You didn't give your password correctly. Please try again.")
                      case _ => StatusLine.showBriefly(ex.toString())
                    }
                }
          })))
        ),
        
        h3("Change Your Display Name"),
        p(s"Your Display Name is currently ${accountInfo.displayName}. Use the form below if you would like to change it."),
        form(div(cls:="form-group col-md-8",
          div(cls:="input-group",
            newDisplayName <= new RxText(cls:="form-control", placeholder:="New Display Name"),
            span(cls:="input-group-btn",
              new ButtonGadget(ButtonGadget.Normal, "Change Display Name", disabled := Rx { passText(newDisplayName)().length() == 0 })({ () =>
                val newName = passText(newDisplayName)()
                Client[UserFunctions].changeDisplayName(newName).call() foreach { userInfo =>
                  UserAccess.setUser(Some(userInfo))
                  PageManager.reload().foreach { newPage => StatusLine.showBriefly(s"Name changed to $newName") }
                }
              })
            )
          )
        ))
      )
  }
    yield PageContents("Your Account", guts)
}