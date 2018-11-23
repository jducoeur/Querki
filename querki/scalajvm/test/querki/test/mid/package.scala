package querki.test

import scala.concurrent.{Await, Awaitable}
import scala.concurrent.duration._

package object mid {
  /**
   * We're not going to try to compose everything beautifully in a Future-centric world.
   * For this test harness, it's usually acceptable to block and wait.
   */
  implicit class Waitable[T](a: Awaitable[T]) {
    def waitFor(): T = Await.result(a, 5 seconds)
  }
}
