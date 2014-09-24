package controllers

import querki.pages.PageIDs._
import querki.spaces.messages.ThingError
import querki.spaces.messages.SpaceError._

class ClientController extends ApplicationBase {
  
  lazy val Tags = interface[querki.tags.Tags]
  
  def thing(ownerId:String, spaceId:String, thingId:String) = withThing(false, ownerId, spaceId, thingId, Some({ 
    case (ThingError(error, stateOpt), rc) if (error.msgName == UnknownName) => {
      // We didn't find the requested Thing, so display a TagThing for it instead:
      val rcWithName = rc.copy(thing = Some(Tags.getTag(thingId, stateOpt.get)))
      Ok(views.html.client(rcWithName, ThingPage, ""))
    }
  })) { implicit rc =>
    // Uncomment this to see details of the Thing we're displaying:
    //QLog.spewThing(rc.thing.getOrElse(rc.state.get))
    // We need to split the lines into a format that Javascript isn't going to choke on, since it can't handle
    // multiline Strings:
    val rendered = 
      rc.thing.get.render(rc).display.toString.replaceAllLiterally("'", "\\'").split('\n').map(line => s"'$line\\n'").mkString(" + \n")
    Ok(views.html.client(rc, ThingPage, rendered))
  }

}
