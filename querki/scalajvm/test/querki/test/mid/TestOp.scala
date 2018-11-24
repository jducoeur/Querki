package querki.test.mid

import scala.concurrent.Future

import cats._
import cats.data._
import cats.effect.IO
import cats.implicits._

object TestOp {
  def pure[T](f: => T): TestOp[T] = StateT.pure[IO, ClientState, T] { f }
  def unit: TestOp[Unit] = StateT.pure[IO, ClientState, Unit] { () }
  
  /**
   * This wraps up the common pattern of a "test operation", which takes a ClientState, and does
   * something Future-y with it.
   */
  def fut[T](f: ClientState => Future[(ClientState, T)]): TestOp[T] = StateT { state =>
    IO.fromFuture(IO(f(state)))
  }
}
