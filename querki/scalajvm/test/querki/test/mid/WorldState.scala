package querki.test.mid

import monocle.Lens
import monocle.macros.GenLens

import querki.data._

/**
 * The state of the world -- the Spaces and Things in them -- from the test harness' point of view.
 */
case class WorldState(
  spaces: Map[TID, SpaceTestState],
  system: SystemStuff
)

object WorldState {
  val empty = WorldState(Map.empty, SystemStuff(None))

  def updateCurrentSpace(f: SpaceTestState => SpaceTestState): TestOp[Unit] = TestOp.update { state =>
    require(state.client.spaceOpt.isDefined)
    val curSpaceId = state.client.spaceOpt.get.oid
    val fUpdatedSpaces = TestState.spacesL.modify { spaces =>
      val space = spaces(curSpaceId)
      val updated = f(space)
      spaces + (curSpaceId -> updated)
    }
    fUpdatedSpaces(state)
  }

  def currentSpace: TestOp[SpaceTestState] = {
    for {
      curSpaceId <- TestOp.fetch(_.client.spaceOpt.get.oid)
      curSpace <- TestOp.fetch(_.world.spaces(curSpaceId))
    } yield curSpace
  }

  /**
   * Fetches the specified Thing from the *current* Space.
   */
  def fetchThing(thingId: TID): TestOp[ThingTestState] = {
    for {
      space <- currentSpace
    } yield space.things(thingId)
  }
}

case class SpaceTestState(
  info: SpaceInfo,
  things: Map[TID, ThingTestState]
)

case class ThingTestState(info: ThingInfo)

case class SystemStuff(typeInfoOpt: Option[AllTypeInfo])
