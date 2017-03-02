package querki.identity

import scalatags.JsDom.all._
import upickle.default._

import querki.comm._
import querki.data.UserInfo
import querki.display._
import querki.globals._
import querki.pages._

class HandleInvitePage(params:ParamMap)(implicit val ecology:Ecology) extends Page("handleInvite")  {
  lazy val invitationString = params.requiredParam("invite")
  
  lazy val UserAccess = interface[UserAccess]
  
  lazy val spaceName = DataAccess.space.map(_.displayName).getOrElse("")
  
  def doInvite():Future[UserInfo] = {
    // We call this one as a raw AJAX call, instead of going through client, since it is a weird case:
    val fut:Future[String] = 
      controllers.LoginController.handleInvite2(
          DataAccess.userName, 
          DataAccess.spaceId.underlying).callAjax(
        "invite" -> invitationString)
        
    fut.map { str =>
      read[UserInfo](str)
    }
  }
  
  def handleInvite():Unit = {
    doInvite().flatMap { userInfo =>
      UserAccess.setUser(Some(userInfo))
      PageManager.showRoot().map { page =>
        if (!UserAccess.isActualUser) {
          // We're not currently logged in
          // Suggest to the user to log in or create an account:
          page.flash(false, 
            """You're logged in as a Guest. If you log into your Querki account, or create one, you will
              |be able to more easily come back here. """.stripMargin,
              new SmallButtonGadget(ButtonGadget.Primary, "Log in / Sign up", id := "_openLoginButton")({() => UserAccess.login() }))
        }
      }
    }
  }
  
  def pageContent = {
    // Kick off the handler in parallel, and don't hold up page rendering for it:
    handleInvite()
    for {
      _ <- Future.successful()
      guts =
        div(
          h1(i(cls:="fa fa-spinner fa-pulse fa-fw"), s"Joining $spaceName...")
        )
    }
      yield PageContents(pageTitleWith("spaceName" -> spaceName), guts)
  }
}
