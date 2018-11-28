package querki.test.mid

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import cats._
import cats.data._
import cats.effect.IO
import cats.implicits._

object TestOp {
  def apply[A](f: TestState => IO[(TestState, A)])(implicit F: Applicative[IO]): TestOp[A] = StateT[IO, TestState, A] { f }
  
  def pure[T](f: => T): TestOp[T] = StateT.pure[IO, TestState, T] { f }
  def unit: TestOp[Unit] = StateT.pure[IO, TestState, Unit] { () }
  
  /**
   * This wraps up the common pattern of a "test operation", which takes a ClientState, and does
   * something Future-y with it.
   */
  def fut[T](f: TestState => Future[(TestState, T)]): TestOp[T] = StateT { state =>
    IO.fromFuture(IO(f(state)))
  }
  
  /**
   * This wraps the common pattern where we want a test operation that is simply an Autowire call.
   */
  def client[T](f: ClientFuncs#ClientBase => Future[T])(implicit cf: ClientFuncs): TestOp[T] = fut { state =>
    val clnt = state.client.spaceOpt match {
      case Some(spaceInfo) => new cf.Client()(ClientContext(spaceInfo), state.client.session)
      case None => new cf.NSClient()(state.client.session)
    }
    val resultFut: Future[T] = f(clnt)
    state.plus(clnt.resultFut) zip resultFut
  }
}
