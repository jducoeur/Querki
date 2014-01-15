package querki.spaces

import models.{OID, Thing}
import models.Thing.PropMap

import querki.ecology._
import querki.util._
import querki.values._

object SpaceChangeMOIDs extends EcotIds(32)

/**
 * This is a general mechanism for allowing Modules to listen in on changes before they take effect.
 */
class SpaceChangeManagerEcot(e:Ecology) extends QuerkiEcot(e) with SpaceChangeManager {
  /**
   * Called before every Create or Modify operation. Listeners can use this specifically to edit the Props.
   * 
   * IMPORTANT: this is called by the Space, so it MUST NOT BLOCK.
   */
  val thingChanges = new ThingChangeUpdater

  class ThingChangeUpdater extends Sequencer[ThingChangeRequest]
}