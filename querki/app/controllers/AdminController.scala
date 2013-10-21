package controllers

import querki.identity._

object AdminController extends ApplicationBase {

  def manageUsers = withUser(true) { rc =>
    val users = User.getAllForAdmin(rc.requesterOrAnon)
    Ok(views.html.manageUsers(rc, users))
  }
  
}