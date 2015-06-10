package controllers

import language.existentials

import scala.concurrent.Future

import play.api._
import play.api.data._
import play.api.data.Forms._
import play.api.mvc._

import models.AsName

import querki.spaces.messages._

import querki.util.TryTrans

class Application extends ApplicationBase {
  
  val newSpaceForm = Form(
    mapping(
      "name" -> nonEmptyText
    )((name) => name)
     ((name:String) => Some(name))
  )
  
  lazy val Core = interface[querki.core.Core]
  lazy val System = interface[querki.system.System]
  
  def scalajsFile(file: String) = Action {
    Ok.sendFile(new java.io.File(s"scalajs/$file"))
  }

  def scalaSharedFile(file: String) = Action {
    Ok.sendFile(new java.io.File(s"scala/$file"))
  }

  def index = withUser(false) { rc =>
    rc.requester match {
      case Some(requester) => {
	    askSpaceMgr[ListMySpacesResponse](ListMySpaces(requester.id)) { 
	      case MySpaces(mySpaces, memberOf) => Ok(views.html.spaces(rc, mySpaces, memberOf))
	    }        
      }
      case _ => Ok(views.html.index(rc))
    }
 }    
  
  // TODO: in the long run, we will want spidering of the main system pages, and allow users to
  // choose to have their pages indexed. We're quite a ways from that, though.
  def robots = Action { implicit request =>
    Ok("""# For the time being, Querki requests that robots stay out. This will change eventually.
        
user-agent: *
disallow: /
""").as("text/plain")
  }

  def spaces = withUser(true) { rc => 
    askSpaceMgr[ListMySpacesResponse](ListMySpaces(rc.requester.get.id)) { 
      case MySpaces(mySpaces, memberOf) => Ok(views.html.spaces(rc, mySpaces, memberOf))
    }
  }

  def newSpace = withUser(true) { implicit rc =>
    if (rc.requesterOrAnon.canOwnSpaces) {
      Ok(views.html.newSpace(rc))
    } else {
      // TODO: internationalize this error message
      doError(routes.Application.index, "You aren't yet allowed to create Spaces")
    }
  }
  
  def doNewSpace = withUser(true) { implicit rc =>
    implicit val request = rc.request
    val requester = rc.requester.get
    newSpaceForm.bindFromRequest.fold(
      errors => doError(routes.Application.newSpace, "You have to specify a legal space name"),
      name => {
        TryTrans[Unit, Future[Result]] { Core.NameProp.validate(name, System.State) }.
          onSucc { _ =>
            SpaceOps.askSpaceManager2(CreateSpace(requester, name)) {
              case SpaceInfo(spaceId, linkName) => {
                val tid = AsName(linkName)
                Redirect(routes.ClientController.thingRedirect(requester.mainIdentity.handle, tid, tid))
              }
              case ThingError(ex, _) => doError(routes.Application.newSpace, ex)
            }
          }.
          onFail { doError(routes.Application.newSpace, _) }.
          result
      }
    )
  }
}
