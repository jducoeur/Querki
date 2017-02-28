package querki.identity

import scalatags.JsDom.all._
import upickle.default._

import querki.comm._
import querki.data.UserInfo
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
    doInvite().map { userInfo =>
      UserAccess.setUser(Some(userInfo))
      PageManager.showRoot()
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
