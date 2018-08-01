package querki.identity

import querki.globals._

class IdentityEcot(e:Ecology) extends ClientEcot(e) with Identity {
  def implements = Set(classOf[Identity])

  lazy val Pages = interface[querki.pages.Pages]
  
  lazy val handleInviteFactory = Pages.registerStandardFactory("_handleInvite", { (params) => new HandleInvitePage(params) })
  lazy val signupFactory = Pages.registerStandardFactory("_signup", { (params) => new SignUpPage(false)(None) })
  lazy val tosFactory = Pages.registerStandardFactory("_tos", { (params) => new TOSPage(None) })
  lazy val validateSignupFactory = Pages.registerStandardFactory("_validateSignup", { (params) => new ValidateSignupPage(params) })

  override def postInit() = {
    handleInviteFactory
    signupFactory
    tosFactory
    validateSignupFactory
  }
}
