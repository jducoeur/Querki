package querki.identity

import scala.concurrent.Future
import org.scalajs.dom

import scalatags.JsDom.all._
import autowire._
import rx._
import org.querki.gadgets._

import querki.display.rx._
import querki.ecology._
import querki.globals._
import querki.pages._
import querki.session.UserFunctions

/**
 * @author jducoeur
 */
class ValidateSignupPage(params:ParamMap)(implicit val ecology:Ecology) extends Page("validateSignup") {
  
  lazy val Client = interface[querki.client.Client]
  lazy val StatusLine = interface[querki.display.StatusLine]
  lazy val UserAccess = interface[UserAccess]
  
  lazy val validationString = params.requiredParam("validate")
  
  lazy val feedback = GadgetRef.of[dom.html.Paragraph]
  
  def pageContent = {
    if (UserAccess.user.isDefined) {
      val guts = div(
        h1(pageTitle),
        
        feedback <=
          p(cls:="text-center", i(cls:="fa fa-spinner fa-pulse fa-5x fa-fw"), span(cls:="sr-only", "Loading..."))
      )
      
      // Fire off the request; this is will redirect if it is successful:
      Client[UserFunctions].validateActivationHash(validationString).call().foreach { 
        if (_) {
          // All set
          PageManager.showIndexPage()
        } else {
          feedback <= p(b("We're sorry, but that isn't a valid activation link for you. Are you logged in as the wrong user?"))
        }
      }
      
      Future.successful(PageContents(guts))
    } else {
      UserAccess.login()
      Future.successful(PageContents(div(p("Please log in, in order to validate your email"))))
    }
  }
}
