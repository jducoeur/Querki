package controllers

import querki.spaces.messages.ThingError
import querki.spaces.messages.SpaceError._

class ClientController extends ApplicationBase {
  
  lazy val Tags = interface[querki.tags.Tags]
  
  def thing(ownerId:String, spaceId:String, thingId:String) = withThing(false, ownerId, spaceId, thingId, Some({ 
    case (ThingError(error, stateOpt), rc) if (error.msgName == UnknownName) => {
      // We didn't find the requested Thing, so display a TagThing for it instead:
      val rcWithName = rc.copy(thing = Some(Tags.getTag(thingId, stateOpt.get)))
      Ok(views.html.client(rcWithName))
    }
  })) { implicit rc =>
    // Uncomment this to see details of the Thing we're displaying:
    //QLog.spewThing(rc.thing.getOrElse(rc.state.get))
    // rc now has all the interesting information copied into it:
    Ok(views.html.client(rc))
  }

}