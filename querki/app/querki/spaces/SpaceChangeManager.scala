package querki.spaces

import models.{OID, Thing}
import models.Thing.PropMap

import querki.util._
import querki.values._

case class ThingChangeRequest(state:SpaceState, modelIdOpt:Option[OID], thingOpt:Option[Thing], newProps:PropMap)

/**
 * This is a general mechanism for allowing Modules to listen in on changes before they take effect.
 */
object SpaceChangeManager {
  /**
   * Called before every Create or Modify operation. Listeners can use this specifically to edit the Props.
   * 
   * IMPORTANT: this is called by the Space, so it MUST NOT BLOCK.
   */
  val thingChanges = new ThingChangeUpdater

  class ThingChangeUpdater extends Sequencer[ThingChangeRequest]
}