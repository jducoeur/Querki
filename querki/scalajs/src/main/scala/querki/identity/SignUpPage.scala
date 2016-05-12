package querki.identity

import scalatags.JsDom.all._

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
  
  def pageContent = for {
    f <- scala.concurrent.Future.successful(true)
    guts = div(
      h1(pageTitle)
    )
  }
    yield PageContents(guts)
}
