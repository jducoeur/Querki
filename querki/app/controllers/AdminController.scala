package controllers

import play.api.Routes
import play.api.mvc._

import querki.identity._
import models._

object AdminController extends ApplicationBase {
  
  def withAdmin(f: RequestContext => Result) = {
    withUser(true) { rc =>
      if (rc.requesterOrAnon.isAdmin)
        f(rc)
      else
        doError(routes.Application.index, "You must be an administrator to use this page")
    }
  } 

  def manageUsers = withAdmin { rc =>
    val users = User.getAllForAdmin(rc.requesterOrAnon)
    Ok(views.html.manageUsers(rc, users))
  }
  
  /**
   * AJAX call to take a Pending User and make them a Free one.
   */
  def upgradePendingUser(userIdStr:String) = withAdmin { rc =>
    val userId = OID(userIdStr)
    val newUserOpt = User.changeUserLevel(userId, rc.requesterOrAnon, UserLevel.FreeUser)
    Ok(newUserOpt.map(_.level.toString).getOrElse(UserLevel.PendingUser.toString))
  }
  
  def javascriptRoutes = Action { implicit request =>
    import routes.javascript._
    Ok(
      Routes.javascriptRouter("adminJsRoutes")(
        routes.javascript.AdminController.upgradePendingUser
      )
    ).as("text/javascript")
  }
  
}