package querki.api

import querki.data._

trait ThingFunctions {
  /**
   * Get the rendered Wikitext for the given Thing.
   * 
   * TODO: this currently returns HTML. It *should* return Wikitext, and get rendered client-side!
   */
  def renderThing(thingId:String):String
}
