package querki.identity

import querki.globals._

class IdentityEcot(e:Ecology) extends ClientEcot(e) with Identity {
  def implements = Set(classOf[Identity])

  lazy val Pages = interface[querki.pages.Pages]
  
  lazy val signupFactory = Pages.registerStandardFactory("_signup", { (params) => new querki.identity.SignUpPage(None) })
  lazy val tosFactory = Pages.registerStandardFactory("_tos", { (params) => new TOSPage(None) })
  lazy val validateSignupFactory = Pages.registerStandardFactory("_validateSignup", { (params) => new querki.identity.ValidateSignupPage(params) })

  override def postInit() = {
    signupFactory
    tosFactory
    validateSignupFactory
  }
}