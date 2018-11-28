package querki.test

import scala.concurrent.{Await, Awaitable}
import scala.concurrent.duration._

import cats.data.StateT
import cats.effect.IO

import play.api.mvc.Result

package object mid {
  /**
   * We're not going to try to compose everything beautifully in a Future-centric world.
   * For this test harness, it's usually acceptable to block and wait.
   */
  implicit class Waitable[T](a: Awaitable[T]) {
    def waitFor(): T = Await.result(a, 5 seconds)
  }
  
  /**
   * The base type of all operations in the mid-test harness.
   */
  type TestOp[T] = StateT[IO, TestState, T]
  
  /**
   * Provides synchronous functions for fetching fields from a Result, suitable for mapping.
   */
  implicit def result2Helpers(result: Result) = new ResultHelpers(result)
}
