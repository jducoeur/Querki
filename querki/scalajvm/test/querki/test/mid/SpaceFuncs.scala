package querki.test.mid

import cats.effect.IO

import autowire._

import querki.data.{SpaceInfo, TID}
import querki.globals._
import querki.session.UserFunctions

/**
 * Provides functions for creating and manipulating Spaces.
 */
trait SpaceFuncs { self: MidTestBase with ClientFuncs =>
  private def addNewSpace(spaceInfo: SpaceInfo): TestOp[Unit] = TestOp { state =>
    IO.pure((TestState.spacesL.modify(_ + (spaceInfo.oid -> SpaceTestState(spaceInfo, Map.empty)))(state), ()))
  }
  
  def setClientSpace(spaceInfo: SpaceInfo): TestOp[Unit] = TestOp { state =>
    IO.pure((TestState.spaceOptL.set(Some(spaceInfo))(state), ()))
  }
  
  def createSpace(name: String): TestOp[TID] = {
    for {
      spaceInfo <- TestOp.client { _[UserFunctions].createSpace(name, None).call() }
      _ <- addNewSpace(spaceInfo)
      _ <- setClientSpace(spaceInfo)
    }
      yield spaceInfo.oid
  }
}
