package querki.api

import querki.data._

trait ThingFunctions {
  /**
   * Get the rendered Wikitext for the given Thing.
   * 
   * TODO: this currently returns HTML. It *should* return Wikitext, and get rendered client-side!
   */
  def renderThing(thingId:String):String
  
  /**
   * Fetch the info for the specified Thing.
   * 
   * Note that, if the named Thing does not exist, that is *not* an error: this will interpret that
   * as a Tag instead.
   */
  def getThingInfo(thingId:String):RequestInfo
}
