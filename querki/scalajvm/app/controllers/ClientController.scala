package controllers

import upickle._

import models.Thing

import querki.pages.ThingPageInfo
import querki.pages.PageIDs._

import querki.spaces.messages.ThingError
import querki.spaces.messages.SpaceError._

class ClientController extends ApplicationBase {
  
  lazy val ClientApi = interface[querki.api.ClientApi]
  lazy val Tags = interface[querki.tags.Tags]
  
  def pageForThing(rc:PlayRequestContext, thing:Thing) = {
    val rendered = thing.render(rc).display.toString
    Ok(views.html.client(rc, ThingPage, write(ThingPageInfo(ClientApi.thingInfo(Some(thing), rc).get, rendered))))    
  }
  
  def thing(ownerId:String, spaceId:String, thingId:String) = withThing(false, ownerId, spaceId, thingId, Some({ 
    case (ThingError(error, stateOpt), rc) if (error.msgName == UnknownName) => {
      // We didn't find the requested Thing, so display a TagThing for it instead:
      val thing = Tags.getTag(thingId, stateOpt.get)
      val rcWithName = rc.copy(thing = Some(thing))
      pageForThing(rcWithName, thing)
    }
  })) { implicit rc =>
    pageForThing(rc, rc.thing.get)
  }
  
//  def renderThing(ownerId:String, spaceId:String, thingId:String) = withThing(false, ownerId, spaceId, thingId)

}
