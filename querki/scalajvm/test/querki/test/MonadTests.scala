package querki.test

// Adapted from:
//   https://github.com/leszekgruchala/typelevel-programming-scala/blob/master/src/main/scala/eu/gruchala/typelevel/hacks/DoNotStickToParticularMonad.scala
// enhanced with bits from
//   https://github.com/scalaz/scalaz/blob/series/7.3.x/core/src/main/scala/scalaz/Monad.scala
// and
//   https://github.com/scalaz/scalaz/blob/series/7.3.x/core/src/main/scala/scalaz/syntax/MonadSyntax.scala
// Originally inspired by http://rea.tech/the-worst-thing-in-our-scala-code-futures/
//
// Adapted to the sort of situation I'm usually dealing with, where I have a class that wants to be instantiated
// with the correct Monad depending on test vs. production.
//
// Also adapted to make MonadLikeOps a value class, which I believe should make this whole thing much cheaper to use.
//
//
// We can and arguably should just use the version of Monad in Scalaz or Cats in real code. This is mainly to prove
// that I kind of understand how this stuff works.
import scala.language.higherKinds
trait MonadLike[F[_]] {
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  def map[A, B](fa: F[A])(f: A => B): F[B]
  def pure[A](a: A): F[A]
  
  val monadSyntax = new MonadLikeSyntax[F] { def F = MonadLike.this }
}

class MonadLikeOps[F[_], A](val self:F[A]) extends AnyVal {
  def flatMap[B](f: A => F[B])(implicit monad:MonadLike[F]): F[B] = {
    monad.flatMap(self)(f)
  }
  def map[B](f: A => B)(implicit monad:MonadLike[F]): F[B] = {
    monad.map(self)(f)
  }
}

trait MonadLikeSyntax[F[_]] {
  implicit def ml2Ops[A](v:F[A]):MonadLikeOps[F,A] = new MonadLikeOps[F,A](v)
  def F:MonadLike[F]
}

object MonadLike {
  implicit def ml2Syntax[F[_]](ml:MonadLike[F]):MonadLikeSyntax[F] = ml.monadSyntax
  implicit def f2Ops[A, F[_]: MonadLike](v:F[A]):MonadLikeOps[F,A] = {
    val monad = implicitly[MonadLike[F]]
    monad.monadSyntax.ml2Ops(v)
  }  
}

object TestMonadLike {
  type Id[A] = A //identity type, clever way to return itself in a typed manner, see also scalaz.Id
  implicit def immediateMonad: MonadLike[Id] = new MonadLike[Id] {
    override def flatMap[A, B](fa: Id[A])(f: (A) => Id[B]): Id[B] = f(fa)
    override def map[A, B](fa: Id[A])(f: (A) => B): Id[B] = f(fa)
    override def pure[A](a: A): Id[A] = a
  }
}

// Just to demonstrate what this would look like with a Future:
object ProductionMonadLike {
  import scala.concurrent.{ExecutionContext, Future}

  implicit def futureMonad(implicit ex: ExecutionContext): MonadLike[Future] = new MonadLike[Future] {
    override def flatMap[A, B](fa: Future[A])(f: (A) => Future[B]): Future[B] = fa.flatMap(f)
    override def map[A, B](fa: Future[A])(f: (A) => B): Future[B] = fa.map(f)
    override def pure[A](a: A): Future[A] = Future.successful(a)
  }
}

import MonadLike._
import TestMonadLike._

/**
 * Scratch test suite, to illustrate how to build as typeclass version of Monad.
 */
class MonadTests extends QuerkiTests {
  "A Monad abstraction" should {
    "work as expected" in {
      class TestClass[F[_]:MonadLike] {
        val monad = implicitly[MonadLike[F]]
        
        def doubler(i:Int):F[Int] = {
          monad.pure(i * 2)
        }
        
        def intToString(i:Int):F[String] = {
          monad.pure(i.toString)
        }
    
        def combiner(i:Int): F[String] = {
          for {
            first <- doubler(i)
            second <- intToString(first)
          }
            yield second
        }
      }
  
      new TestClass[Id].combiner(4) should equal ("8")      
    }
  }
}