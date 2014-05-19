package controllers

import play.api._
import play.api.mvc._

import querki.core.QLText

class ExploreController extends ApplicationBase {
  
  lazy val QL = interface[querki.ql.QL]
  
  def showExplorer(ownerId:String, spaceId:String, thingId:String) = withThing(true, ownerId, spaceId, thingId) { implicit rc =>
    Ok(views.html.explorer(rc))
  }
  
  def evaluate(ownerId:String, spaceId:String, thingId:String, ql:String) = withThing(true, ownerId, spaceId, thingId) { implicit rc =>
    if (AccessControl.isMember(rc.requesterOrAnon, rc.state.get)) {
      val context = rc.thing.get.thisAsContext
      val result = QL.processMethod(QLText(ql), context, None, Some(rc.thing.get)).wikify(context).display.toString
      Ok(result)
    } else {
      Unauthorized("Only Members of the Space are allowed to use Querki Explorer")
    }
  }

  def javascriptRoutes = Action { implicit request =>
    import routes.javascript._
    Ok(
      Routes.javascriptRouter("jsExploreRoutes")(
        routes.javascript.ExploreController.evaluate
      )
    ).as("text/javascript")
  }

}