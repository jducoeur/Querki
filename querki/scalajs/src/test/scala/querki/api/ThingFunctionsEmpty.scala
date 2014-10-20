package querki.api

import models.Wikitext
import querki.data.RequestInfo
import querki.pages.ThingPageDetails

class ThingFunctionsEmpty extends ThingFunctions {
  def getRequestInfo():RequestInfo = ???
  def getThingPage(thingId:String):ThingPageDetails = ???
}
