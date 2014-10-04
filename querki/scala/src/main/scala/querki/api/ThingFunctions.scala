package querki.api

import models.{DisplayText, Wikitext}
import querki.data._

trait ThingFunctions {
  /**
   * Get the rendered Wikitext for the given Thing.
   */
  def renderThing(thingId:String):Wikitext
  
  /**
   * Fetch the info for the specified Thing.
   * 
   * Note that, if the named Thing does not exist, that is *not* an error: this will interpret that
   * as a Tag instead.
   */
  def getThingInfo(thingId:String):RequestInfo
}
