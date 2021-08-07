package querki.publication

import autowire._

import querki.data._
import querki.globals.execContext
import querki.test.mid._

import AllFuncs._

/**
 * Not included in AllFuncs, since the functions here are relatively specialized and I don't want to pollute that
 * namespace too much. Import or extend this explicitly if you want these.
 */
trait PublicationMidFuncs {

  ///////////////////////////////////
  //
  // Wrappers around the PublicationFunctions API.
  //

  def publish(thingId: TID): TestOp[ThingInfo] =
    TestOp.client { _[PublicationFunctions].publish(thingId).call() }

  def update(
    thingId: TID,
    minor: Boolean
  ): TestOp[ThingInfo] =
    TestOp.client { _[PublicationFunctions].update(thingId, minor).call() }

  def changePublishedModels(): TestOp[Unit] =
    TestOp.client { _[PublicationFunctions].changePublishedModels().call() }

  def discardChanges(thingId: TID): TestOp[Unit] =
    TestOp.client { _[PublicationFunctions].discardChanges(thingId).call() }
}

object PublicationMidFuncs extends PublicationMidFuncs
