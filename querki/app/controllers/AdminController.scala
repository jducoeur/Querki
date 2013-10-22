package controllers

import play.api.mvc._

import querki.identity._

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
  
}