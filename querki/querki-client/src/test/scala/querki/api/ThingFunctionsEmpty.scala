package querki.api

import models.Wikitext
import querki.data.{RequestInfo, ThingInfo}
import querki.pages.ThingPageDetails

class ThingFunctionsEmpty extends ThingFunctions {
  def getRequestInfo():RequestInfo = ???
  def getThingPage(thingId:String):ThingPageDetails = ???
  def getThingInfo(thingId:String):ThingInfo = ???
  def evaluateQL(thingId:String, ql:String):Wikitext = ???
}
