package querki.test.mid

import autowire._

import querki.api._
import querki.data._
import querki.globals._

import AllFuncs._

trait ThingFuncs {
  def getThingInfo(thingId: TID): TestOp[ThingInfo] = {
    for {
      info <- TestOp.client { _[ThingFunctions].getThingInfo(thingId).call() }
    }
      yield info
  }
}
