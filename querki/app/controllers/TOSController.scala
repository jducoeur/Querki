package controllers

import play.api.data._
import play.api.data.Forms._
import play.api.mvc._

import querki.system.TOSModule

import querki.util._

/**
 * Handle Terms of Service. This class works hand-in-glove with querki.system.TOSModule.
 */
object TOSController extends ApplicationBase {
  case class TOSForm(version:Int = -1, agreed:Boolean = false)
  val tosForm = Form(
    mapping(
      "version" -> number,
      "agreed" -> checked("Please accept the terms and conditions")
    )(TOSForm.apply)(TOSForm.unapply)
  )

  def showTOS = withUser(true) { rc =>
    Ok(views.html.tos(rc, tosForm.fill(TOSForm())))
  }
  
  def handleTOS = withUser(true) { implicit rc =>
    implicit val request = rc.request
    val rawForm = tosForm.bindFromRequest
    // Note that agreed is not sent back if it is not checked:
    val response = rawForm.data.get("agreed").map(_.toLowerCase()).getOrElse("false")
    // Yes, this ought to be using the usual form-matching capability. But I cannot, for the life of me,
    // get the damned checkbox to be recognized properly. Far as I can tell, Play doesn't want to accept
    // the bog-standard "on" response from the checkbox:
    if (response == "on" || response == "true") {
      // TODO: use redirectTo to return to where we were before this started.
      Tryer[querki.identity.User, PlainResult]
        { TOSModule.recordAccept(rc.requesterOrAnon, rawForm.data("version").toInt) }
        { user => Redirect(routes.Application.index)}
        { ex => doError(routes.TOSController.showTOS, ex) }
    } else {
      doError(routes.TOSController.showTOS, "Please accept the terms and conditions")
    }
  }
}
