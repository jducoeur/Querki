package querki.test

import querki.globals._
import akka.actor._

class TestActor extends Actor {
  def receive = {
    case something => QLog.spew(s"TestActor got $something")
  }
}

/**
 * This is a place to put little, temporary tests. Note that these should normally be
 * marked ignore when checked in.
 * 
 * @author jducoeur
 */
class ScratchTests extends QuerkiTests
{
  "I" should {
    "be able to get a password" ignore {
      val Encryption = interface[querki.security.Encryption]
      
      val plaintext = "testing"
      val hash = Encryption.calcHash(plaintext)
      val auth = Encryption.authenticate(plaintext, hash)
      println(s"""The encrypted form of $plaintext is '$hash'; authenticated = $auth""")
    }
    
    "be able to identify an ActorRef as such" ignore {
      
      val anyRefClass = classOf[Object]
      def collectClasses(clazz:Class[_]):Set[Class[_]] = {
        def collectRec(sup:Class[_]):Set[Class[_]] = {
          if (sup == null || sup == anyRefClass)
            Set.empty
          else
            collectClasses(sup) + sup
        }
        
        (collectRec(clazz.getSuperclass) /: clazz.getInterfaces) { (set, interf) =>
          set ++ collectRec(interf)
        }
      }
      
      val actorSystem = ActorSystem()
      val ref = actorSystem.actorOf(Props(classOf[TestActor]))
      val clazz = ref.getClass
      QLog.spew(s"The ActorRef is actually a $clazz")
      QLog.spew(s"The full collection of superclasses are:")
      collectClasses(clazz).foreach { sup =>
        QLog.spew(s"    $sup")
      }
    }
    
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
    //
    // We can and arguably should just use the version of Monad in Scalaz or Cats in real code. This is mainly to prove
    // that I kind of understand how this stuff works.
    "be able to define an abstraction of Future" in {
      import scala.language.higherKinds
      trait MonadLike[F[_]] {
        def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
        def map[A, B](fa: F[A])(f: A => B): F[B]
        def pure[A](a: A): F[A]
        
        val monadSyntax = new MonadLikeSyntax[F] { def F = MonadLike.this }
      }
      
      class MonadLikeOps[F[_], A](val self:F[A])(implicit val F:MonadLike[F]) {
        def flatMap[B](f: A => F[B]): F[B] = F.flatMap(self)(f)
        def map[B](f: A => B): F[B] = F.map(self)(f)
      }
      
      trait MonadLikeSyntax[F[_]] {
        implicit def ml2Ops[A](v:F[A]):MonadLikeOps[F,A] = new MonadLikeOps[F,A](v)(MonadLikeSyntax.this.F)
        def F:MonadLike[F]
      }
      
      implicit def ml2Syntax[F[_]](ml:MonadLike[F]):MonadLikeSyntax[F] = ml.monadSyntax
      implicit def f2Ops[A, F[_]: MonadLike](v:F[A]):MonadLikeOps[F,A] = {
        val monad = implicitly[MonadLike[F]]
        monad.monadSyntax.ml2Ops(v)
      }
      
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
    
      // Just to demonstrate what this would look like with a Future:
      object InProduction {
        import scala.concurrent.{ExecutionContext, Future}
    
        implicit def futureMonad(implicit ex: ExecutionContext): MonadLike[Future] = new MonadLike[Future] {
          override def flatMap[A, B](fa: Future[A])(f: (A) => Future[B]): Future[B] = fa.flatMap(f)
          override def map[A, B](fa: Future[A])(f: (A) => B): Future[B] = fa.map(f)
          override def pure[A](a: A): Future[A] = Future.successful(a)
        }
    
        //new TestClass[Future].combiner(4)
      }
    
      type Id[A] = A //identity type, clever way to return itself in a typed manner, see also scalaz.Id
      implicit def immediateMonad: MonadLike[Id] = new MonadLike[Id] {
        override def flatMap[A, B](fa: Id[A])(f: (A) => Id[B]): Id[B] = f(fa)
        override def map[A, B](fa: Id[A])(f: (A) => B): Id[B] = f(fa)
        override def pure[A](a: A): Id[A] = a
      }
  
      new TestClass[Id].combiner(4) should equal ("8")
    }
  }
}

