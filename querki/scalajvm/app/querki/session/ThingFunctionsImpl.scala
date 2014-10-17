package querki.session

import models.{DisplayText, Thing, ThingId, Wikitext}

import querki.globals._

import querki.api.ThingFunctions
import querki.data.RequestInfo
import querki.pages.ThingPageDetails

trait ThingFunctionsImpl extends SessionApiImpl with ThingFunctions {
  
  def ClientApi:querki.api.ClientApi
  lazy val HtmlUI = interface[querki.html.HtmlUI]
  
  def getRequestInfo():RequestInfo = ClientApi.requestInfo(rc)

  def getThingPage(thingId:String):ThingPageDetails = withThing(thingId) { thing =>
    implicit val state = rc.state.get
    
    val thingInfo = ClientApi.thingInfo(thing, rc)
    // Note that both the root Thing and (more importantly) TagThings won't have a Model:
    val modelInfo = for {
      model <- thing.getModelOpt
    } 
      yield ClientApi.thingInfo(model, rc)
    
    val customHeaderOpt = for {
      pv <- thing.getPropOpt(HtmlUI.PageHeaderProperty)
    }
      yield pv.v.wikify(thing.thisAsContext(rc))

    val rendered = thing.render(rc)
    
    ThingPageDetails(thingInfo, modelInfo, customHeaderOpt, rendered)
  }
}
