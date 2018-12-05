package querki.test.mid

import monocle.Lens
import monocle.macros.GenLens

import querki.data._

/**
 * The state of the world -- the Spaces and Things in them -- from the test harness' point of view.
 */
case class WorldState(spaces: Map[TID, SpaceTestState])

object WorldState {
  lazy val empty = WorldState(Map.empty)
}

case class SpaceTestState(
  info: SpaceInfo,
  things: Map[TID, ThingTestState]
)

case class ThingTestState(info: ThingInfo)
