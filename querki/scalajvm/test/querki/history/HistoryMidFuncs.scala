package querki.history

import autowire._

import querki.data.{SpaceInfo, TID, ThingInfo}
import querki.globals._
import querki.history.HistoryFunctions.{HistorySummary, HistoryVersion}
import querki.test.mid._

trait HistoryMidFuncs {

  ////////////////
  //
  // Implementation of HistoryFunctions
  //

  /**
   * Fetch the complete history of this Space.
   */
  def getHistorySummary(): TestOp[HistorySummary] =
    TestOp.client { _[HistoryFunctions].getHistorySummary().call() }

  /**
   * Rolls this Space back to the specified version. The UI should do confirmation first!
   */
  def rollbackTo(v: HistoryVersion): TestOp[SpaceInfo] =
    TestOp.client { _[HistoryFunctions].rollbackTo(v).call() }

  /**
   * Restores the specified Thing, as of the last version where it existed.
   *
   * We assume that the given TID is specifically from the oid field.
   */
  def restoreDeletedThing(tid: TID): TestOp[ThingInfo] =
    TestOp.client { _[HistoryFunctions].restoreDeletedThing(tid).call() }

  //////////////////////////
  //
  // Utility functions
  //

  def withHistoryVersion[T](v: HistoryVersion)(op: TestOp[T]): TestOp[T] = {
    for {
      oldClientState <- TestOp.fetch(_.client)
      _ <- TestOp.update(state =>
        TestState.clientL.modify { clientState =>
          clientState.copy(currentPageParams =
            clientState.currentPageParams + (HistoryFunctions.viewingHistoryParam -> v.toString))
        }(state)
      )
      t <- op
      _ <- TestOp.update(state => TestState.clientL.set(oldClientState)(state))
    } yield t
  }
}

object HistoryMidFuncs extends HistoryMidFuncs
