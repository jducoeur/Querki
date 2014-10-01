package querki.session

import models.{Thing, ThingId}

import querki.global._

import querki.api.ThingFunctions
import querki.data.RequestInfo
import querki.values.RequestContext

trait ThingFunctionsImpl extends SessionApiImpl with ThingFunctions {
  
  lazy val ClientApi = interface[querki.api.ClientApi]
  lazy val Tags = interface[querki.tags.Tags]
  
  def withThing[R](thingId:String)(f:(Thing, RequestContext) => R):R = {
    val oid = ThingId(thingId)
    // Either show this actual Thing, or a synthetic TagThing if it's not found:
    val thing = state.anything(oid).getOrElse(Tags.getTag(thingId, state))
    f(thing, rc + thing)
  }
  
  def renderThing(thingId:String):String = {
    withThing(thingId) { (thing, rc) =>
      thing.render(rc).display.toString    
    }
  }
  
  def getThingInfo(thingId:String):RequestInfo = {
    withThing(thingId) { (thing, rc) =>
      ClientApi.requestInfo(rc)
    }
  }
}
