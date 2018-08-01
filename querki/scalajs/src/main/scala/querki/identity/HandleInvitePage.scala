package querki.identity

import org.scalajs.dom.html

import scalatags.JsDom.all._
import upickle.default._

import org.querki.gadgets._

import querki.comm._
import querki.data.UserInfo
import querki.display._
import querki.globals._
import querki.pages._

class HandleInvitePage(params:ParamMap)(implicit val ecology:Ecology) extends Page("handleInvite")  {
  lazy val invitationString = params.requiredParam("invite")
  // This is set in the server's InvitationNotifierEcot.makeInviteLink. It should only apply to
  // shared invites; if it is set to true, then we need to get them logged in *before* we go
  // into invite-acceptance:
  lazy val requiresMembership = params.get("reqMemb").map(_ == "true").getOrElse(false)
  
  lazy val StatusLine = interface[querki.display.StatusLine]
  lazy val UserAccess = interface[UserAccess]
  
  lazy val spaceName = DataAccess.space.map(_.displayName).getOrElse("")
  
  def doInvite(invite: String):Future[Option[UserInfo]] = {
    // We call this one as a raw AJAX call, instead of going through client, since it is a weird case:
    val fut:Future[String] = 
      controllers.LoginController.handleInvite2(
          DataAccess.userName, 
          DataAccess.spaceId.underlying).callAjax(
        "invite" -> invite)
        
    fut.map { str =>
      if (str.isEmpty) {
        None
      } else {
        Some(read[UserInfo](str))
      }
    }
  }
  
  def handleInvite(invite: String): Future[Boolean] = {
    doInvite(invite).flatMap { userInfoOpt =>
      userInfoOpt match {
        case Some(userInfo) => {
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
            
            true
          }          
        }
        
        case None => {
          Future.successful(false)
        }
      }
    }
  }
  
  def startInviteProcess(): Future[Boolean] = {
    if (requiresMembership && !(UserAccess.isActualUser)) {
      // This invitation is only open to Querki members, and we don't currently have one. So toss
      // them over to SignInOrUp first, and deal with the invitation afterwards:
      SignUpPage.run(includeSignin = true).flatMap(_ => handleInvite(invitationString))
    } else
      // Normal case -- just begin the invitation:
      handleInvite(invitationString)
  }
  
  val displayDiv = GadgetRef.of[html.Div]
  
  def pageContent = {
    // Kick off the handler in parallel, and don't hold up page rendering for it:
    val inviteFut = startInviteProcess()
    inviteFut.onSuccess {
      case true => // No-op -- we've already side-effected the result. This might want to be changed
      case false => displayDiv <= div(h3("Unable to join"), p("Sorry -- that doesn't appear to be a currently-valid invitation."))
    }
    for {
      _ <- Future.successful()
      guts =
        div(
          displayDiv <= div(
            h1(i(cls:="fa fa-spinner fa-pulse fa-fw"), s"Joining $spaceName...")
          )
        )
    }
      yield PageContents(pageTitleWith("spaceName" -> spaceName), guts)
  }
}
