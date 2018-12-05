package querki.test.mid

import cats.effect.IO

import autowire._

import querki.data._
import querki.editing._
import querki.editing.EditFunctions._
import querki.globals._

trait EditFuncs { self: ClientFuncs =>
  
  /**
   * Given a newly-created Thing, put it into the current Space.
   */
  def addNewThing(info: ThingInfo): TestOp[Unit] = TestOp { state =>
    require(state.client.spaceOpt.isDefined)
    val curSpaceId = state.client.spaceOpt.get.oid
    val fUpdatedSpaces = TestState.spacesL.modify { spaces =>
      val space = spaces(curSpaceId)
      val updated = space.copy(things = (space.things + (info.oid -> ThingTestState(info))))
      spaces + (curSpaceId -> updated)
    }
    IO.pure((fUpdatedSpaces(state), ()))
  }

  def createThing(modelId:TID, initialProps:Seq[PropertyChange] = Seq.empty): TestOp[TID] = {
    for {
      info <- TestOp.client { _[EditFunctions].create(modelId, initialProps).call() }
      _ <- addNewThing(info)
    }
      yield info.oid
  }
}
