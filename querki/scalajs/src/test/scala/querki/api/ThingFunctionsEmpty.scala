package querki.api

import models.Wikitext
import querki.data._

class ThingFunctionsEmpty extends ThingFunctions {
  def renderThing(thingId:String):Wikitext = ???
  def getThingInfo(thingId:String):RequestInfo = ???
}
