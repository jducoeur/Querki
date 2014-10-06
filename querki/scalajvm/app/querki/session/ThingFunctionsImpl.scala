package querki.session

import models.{DisplayText, Thing, ThingId, Wikitext}

import querki.global._

import querki.api.ThingFunctions
import querki.data.RequestInfo
import querki.pages.ThingPageDetails
import querki.values.RequestContext

trait ThingFunctionsImpl extends SessionApiImpl with ThingFunctions {
  
  lazy val ClientApi = interface[querki.api.ClientApi]
  lazy val HtmlUI = interface[querki.html.HtmlUI]
  lazy val Tags = interface[querki.tags.Tags]
  
  def withThing[R](thingId:String)(f:(Thing, RequestContext) => R):R = {
    val oid = ThingId(thingId)
    // Either show this actual Thing, or a synthetic TagThing if it's not found:
    val thing = state.anything(oid).getOrElse(Tags.getTag(thingId, state))
    f(thing, rc + thing)
  }
  
  def renderThing(thingId:String):Wikitext = {
    withThing(thingId) { (thing, rc) =>
      thing.render(rc)
    }
  }
  
  def getThingInfo(thingId:String):RequestInfo = {
    withThing(thingId) { (thing, rc) =>
      implicit val state = rc.state.get
      val pageHeaderOpt = for {
        pv <- thing.getPropOpt(HtmlUI.PageHeaderProperty)
      }
        yield pv.v.wikify(thing.thisAsContext(rc))
        
      ClientApi.requestInfo(rc, ThingPageDetails(pageHeaderOpt))
    }
  }
}
