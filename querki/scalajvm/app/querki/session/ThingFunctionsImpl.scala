package querki.session

import models.{DisplayText, Thing, ThingId, Wikitext}

import querki.globals._

import querki.api.ThingFunctions
import querki.core.QLText
import querki.data.{RequestInfo, ThingInfo}
import querki.pages.ThingPageDetails

trait ThingFunctionsImpl extends SessionApiImpl with ThingFunctions {
  
  def ClientApi:querki.api.ClientApi
  lazy val HtmlUI = interface[querki.html.HtmlUI]
  lazy val QL = interface[querki.ql.QL]
  
  def getRequestInfo():RequestInfo = ClientApi.requestInfo(rc)
  
  def getThingInfo(thingId:String) = withThing(thingId) { thing =>
    ClientApi.thingInfo(thing, rc)
  }

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
  
  def evaluateQL(thingId:String, ql:String):Wikitext = withThing(thingId) { thing =>
    implicit val r = rc
    val context = thing.thisAsContext
    QL.processMethod(QLText(ql), context, None, Some(thing)).wikify(context)
  }
}
