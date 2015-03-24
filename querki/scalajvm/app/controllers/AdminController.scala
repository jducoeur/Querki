package controllers

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits._

import play.api.Routes
import play.api.data._
import play.api.data.Forms._
import play.api.mvc._

import querki.ecology._
import querki.identity._
import models._

class AdminController extends ApplicationBase {
  
  lazy val AdminOps = interface[querki.admin.AdminOps]
  lazy val Email = interface[querki.email.Email]
  
  case class SystemMessageForm(header:String, body:String)
  val systemMessageForm = Form(
    mapping(
      "header" -> nonEmptyText,
      "body" -> nonEmptyText
    )((header, body) => SystemMessageForm(header, body))
     ((form:SystemMessageForm) => Some((form.header, form.body)))
  )
  
  def withAdmin(f: PlayRequestContext => Future[Result]) = {
    withUser(true) { rc =>
      if (rc.requesterOrAnon.isAdmin)
        f(rc)
      else
        doError(routes.Application.index, "You must be an administrator to use this page")
    }
  }

  def withSuperadmin(f: PlayRequestContext => Future[Result]) = {
    withUser(true) { rc =>
      if (rc.requesterOrAnon.level == UserLevel.SuperadminUser)
        f(rc)
      else
        doError(routes.Application.index, "You must be the super-administrator to use this page")
    }
  }

  def manageUsers = withAdmin { rc =>
    val users = UserAccess.getAllForAdmin(rc.requesterOrAnon)
    Ok(views.html.manageUsers(rc, users))
  }
  
  def showSpaceStatus = withAdmin { rc =>
    AdminOps.getSpacesStatus(rc.requesterOrAnon) { status =>
        val sortedSpaces = status.spaces.sortBy(_.name)
        val spaceDisplays = sortedSpaces.map { spaceStatus => s"* **${spaceStatus.name}:** ${spaceStatus.thingConvs} active ThingConversations; ${spaceStatus.nSessions} active Sessions" }
        val contents = Wikitext(s"""# ADMIN: Current Space Status
            |
            |------
            |
            |The following Spaces are currently live:
            |
            |${spaceDisplays.mkString("\n")}""".stripMargin)
        Ok(views.html.main(QuerkiTemplate.Admin, "Space Status", rc)(contents.display.html))
    }
  }
  
  /**
   * AJAX call to take a Pending User and make them a Free one.
   */
  def upgradePendingUser(userIdStr:String) = withAdmin { rc =>
    val userId = OID(userIdStr)
    UserAccess.changeUserLevel(userId, rc.requesterOrAnon, UserLevel.FreeUser).map { newUserOpt =>
      newUserOpt.map { newUser =>
        // TODO: these strings should be internationalized
        // TODO: re-examine this email when we get to Beta.
        val subject = Wikitext("Welcome to full membership in Querki!")
        val body = Wikitext(s"""<p>Your Querki account, ${newUser.mainIdentity.handle}, has been approved for all features. 
          |This means that you can now create Spaces of your own, and share them with your friends. <a href="http://www.querki.net/">Log into Querki</a>, or
          |click on the "Querki" icon in the upper-left corner of the page if you are already logged in,
          |to create a new Space.</p>
          |<p>Remember, Querki is still in "Beta", which means that there are still some bugs, and lots of features are yet to be
          |implemented. But we hope there is enough there now for you to find it useful.</p>
          |<p>Have fun, and please contact us if you need any help!</p>""".stripMargin)
        Email.sendSystemEmail(newUser.mainIdentity, subject, body)
      }
    
      Ok(newUserOpt.map(_.level.toString).getOrElse(UserLevel.PendingUser.toString))
    }
  }
  
  /**
   * AJAX call to make a user into an admin. Can only be used by the superadmin account.
   */
  def makeAdmin(userIdStr:String) = withSuperadmin { rc =>
    val userId = OID(userIdStr)
    UserAccess.changeUserLevel(userId, rc.requesterOrAnon, UserLevel.AdminUser).map { newUserOpt =>
      Ok(newUserOpt.map(_.level.toString).getOrElse(UserLevel.PendingUser.toString))
    }
  }
  
  def sendSystemMessage = withAdmin { rc =>
    Ok(views.html.systemMessage(rc))
  }
  
  // TODO: in the long run, there is *way* too much boilerplate for this feature! We have a Play template; a
  // controller; a function in the AdminEcot; and a message to the AdminActor; all just to call Notifications.
  // Instead, we should be looking for a standard way for the webpage to send a message directly to the AdminActor,
  // without all the intervening BS. Figure this out once we begin to rewrite the UI in Scala -- this is a great
  // candidate for an early rewrite. (But make sure we are checking credentials somewhere!)
  def doSendSystemMessage = withAdmin { rc =>
    implicit val request = rc.request
    val rawForm = systemMessageForm.bindFromRequest
    rawForm.fold(
      errorForm => doError(routes.AdminController.sendSystemMessage, "That isn't a legal system message"),
      msg => {
        // TODO: parse the message, show it to the Admin, and get confirmation before sending
        AdminOps.sendSystemMessage(rc.requesterOrAnon, msg.header, msg.body)
        Redirect(routes.Application.index).flashing("info" -> "System Message sent")
      }
    )
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