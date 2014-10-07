package querki.session

import models.{DisplayText, Thing, ThingId, Wikitext}

import querki.globals._

import querki.api.ThingFunctions
import querki.data.RequestInfo
import querki.pages.ThingPageDetails

trait ThingFunctionsImpl extends SessionApiImpl with ThingFunctions {
  
  lazy val ClientApi = interface[querki.api.ClientApi]
  lazy val HtmlUI = interface[querki.html.HtmlUI]
  
  def renderThing(thingId:String):Wikitext = withThing(thingId) { thing =>
    thing.render(rc)
  }

  def getThingInfo(thingId:String):RequestInfo = withThing(thingId) { thing =>
    implicit val state = rc.state.get
    val pageHeaderOpt = for {
      pv <- thing.getPropOpt(HtmlUI.PageHeaderProperty)
    }
      yield pv.v.wikify(thing.thisAsContext(rc))
        
    ClientApi.requestInfo(rc, ThingPageDetails(pageHeaderOpt))
  }
}
