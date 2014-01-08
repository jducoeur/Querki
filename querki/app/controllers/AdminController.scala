package controllers

import play.api.Routes
import play.api.mvc._

import querki.ecology._
import querki.identity._
import models._

class AdminController extends ApplicationBase {
  
  lazy val Email = interface[querki.email.Email]
  
  def withAdmin(f: PlayRequestContext => Result) = {
    withUser(true) { rc =>
      if (rc.requesterOrAnon.isAdmin)
        f(rc)
      else
        doError(routes.Application.index, "You must be an administrator to use this page")
    }
  } 

  def withSuperadmin(f: PlayRequestContext => Result) = {
    withUser(true) { rc =>
      if (rc.requesterOrAnon.level == UserLevel.SuperadminUser)
        f(rc)
      else
        doError(routes.Application.index, "You must be the super-administrator to use this page")
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
    
    newUserOpt.map { newUser =>
      // TODO: these strings should be internationalized
      // TODO: re-examine this email when we get to Beta.
      val subject = Wikitext("Welcome to full membership in Querki!")
      val body = Wikitext(s"""Your Querki account, ${newUser.mainIdentity.handle}, has been approved for all features. 
        |This means that you can now create Spaces of your own, and share them with your friends. Go to the "Your Spaces"
        |page (which you can get to by clicking on "Logged in as ${newUser.mainIdentity.name}" in the upper-right-hand corner)
        |to create a new Space.
        |
        |Remember, Querki is still in "Alpha", which means that there are still some bugs, and lots of features are yet to be
        |implemented. But we hope there is enough there now for you to find it useful.
        |
        |Have fun, and please contact us if you need any help!""".stripMargin)
      Email.sendSystemEmail(newUser.mainIdentity, subject, body)
    }
    
    Ok(newUserOpt.map(_.level.toString).getOrElse(UserLevel.PendingUser.toString))
  }
  
  /**
   * AJAX call to make a user into an admin. Can only be used by the superadmin account.
   */
  def makeAdmin(userIdStr:String) = withSuperadmin { rc =>
    val userId = OID(userIdStr)
    val newUserOpt = User.changeUserLevel(userId, rc.requesterOrAnon, UserLevel.AdminUser)
    Ok(newUserOpt.map(_.level.toString).getOrElse(UserLevel.PendingUser.toString))
  }
  
  def javascriptRoutes = Action { implicit request =>
    import routes.javascript._
    Ok(
      Routes.javascriptRouter("adminJsRoutes")(
        routes.javascript.AdminController.upgradePendingUser,
        routes.javascript.AdminController.makeAdmin
      )
    ).as("text/javascript")
  }
  
}