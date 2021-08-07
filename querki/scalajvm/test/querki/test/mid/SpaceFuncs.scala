package querki.test.mid

import cats.effect.IO
import autowire._
import org.scalatest.Matchers._
import org.scalactic.source.Position
import querki.data.{SpaceInfo, TID}
import querki.globals._
import querki.session.UserFunctions
import AllFuncs._
import querki.spaces.SpaceOps

/**
 * Provides functions for creating and manipulating Spaces.
 */
trait SpaceFuncs {

  private def addNewSpace(spaceInfo: SpaceInfo): TestOp[Unit] = TestOp { state =>
    IO.pure((TestState.spacesL.modify(_ + (spaceInfo.oid -> SpaceTestState(spaceInfo, Map.empty)))(state), ()))
  }

  def setClientSpace(spaceInfo: SpaceInfo): TestOp[Unit] = TestOp { state =>
    IO.pure((TestState.spaceOptL.set(Some(spaceInfo))(state), ()))
  }

  def clientSpaceId(): TestOp[TID] = {
    TestOp.fetch(_.curSpace.info.oid)
  }

  def createSpace(name: String): TestOp[TID] = {
    for {
      spaceInfo <- TestOp.client { _[UserFunctions].createSpace(name, None).call() }
      _ <- addNewSpace(spaceInfo)
      _ <- setClientSpace(spaceInfo)
    } yield spaceInfo.oid
  }

  def expectingFullFiltering[R](op: TestOp[R])(implicit position: Position): TestOp[R] = {
    // EVIL!!! Can we substitute in a cats-effect Var instead?
    var gotEvolution = false
    for {
      _ <- TestOp.withState(_.harness.ecology.api[SpaceOps].registerFullEvolutionListener(() => gotEvolution = true))
      result <- op
      _ = assert(gotEvolution, "Expected this operation to do an expensive full filtering, but it didn't!")
    } yield result
  }

  def expectingEfficientEvolution[R](op: TestOp[R])(implicit position: Position): TestOp[R] = {
    // EVIL!!! Can we substitute in a cats-effect Var instead?
    var gotEvolution = false
    for {
      _ <- TestOp.withState(_.harness.ecology.api[SpaceOps].registerFullEvolutionListener(() => gotEvolution = true))
      result <- op
      _ = assert(!gotEvolution, "This operation did an unexpected (expensive) full filter!")
    } yield result
  }
}

object SpaceFuncs extends SpaceFuncs
