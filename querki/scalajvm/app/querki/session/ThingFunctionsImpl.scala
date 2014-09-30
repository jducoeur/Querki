package querki.session

import models.ThingId

import querki.api.ThingFunctions

trait ThingFunctionsImpl extends SessionApiImpl with ThingFunctions {
  
  lazy val Tags = interface[querki.tags.Tags]
  
  def renderThing(thingId:String):String = {
    val oid = ThingId(thingId)
    // Either show this actual Thing, or a synthetic TagThing if it's not found:
    val thing = state.anything(oid).getOrElse(Tags.getTag(thingId, state))
    thing.render(rc + thing).display.toString
  }
  
}
