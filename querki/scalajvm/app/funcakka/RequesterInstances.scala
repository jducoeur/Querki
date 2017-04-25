package cats
package instances

import scala.util.control.NonFatal

import org.querki.requester._

/**
 * Cats-style Instances for RequestM. Deliberately apes the implementation of FutureInstances.
 */
trait RequestMInstances extends RequestMInstances1 {

  implicit val catsStdInstancesForRequestM: MonadError[RequestM, Throwable] with CoflatMap[RequestM] with Monad[RequestM] =
    new RequestMCoflatMap with MonadError[RequestM, Throwable] with Monad[RequestM] {
      def pure[A](x: A): RequestM[A] = RequestM.successful(x)

      def flatMap[A, B](fa: RequestM[A])(f: A => RequestM[B]): RequestM[B] = fa.flatMap(f)

      /**
       * Note that while this implementation will not compile with `@tailrec`,
       * it is in fact stack-safe.
       */
      final def tailRecM[B, C](b: B)(f: B => RequestM[Either[B, C]]): RequestM[C] =
        f(b).flatMap {
          case Left(b1) => tailRecM(b1)(f)
          case Right(c) => RequestM.successful(c)
        }

      def handleErrorWith[A](fea: RequestM[A])(f: Throwable => RequestM[A]): RequestM[A] = fea.recoverWith { case t => f(t) }

      def raiseError[A](e: Throwable): RequestM[A] = RequestM.failed(e)
      override def handleError[A](fea: RequestM[A])(f: Throwable => A): RequestM[A] = fea.recover { case t => f(t) }

      override def attempt[A](fa: RequestM[A]): RequestM[Either[Throwable, A]] =
        (fa.map(a => Right[Throwable, A](a))) recover { case NonFatal(t) => Left(t) }

      override def recover[A](fa: RequestM[A])(pf: PartialFunction[Throwable, A]): RequestM[A] = fa.recover(pf)

      override def recoverWith[A](fa: RequestM[A])(pf: PartialFunction[Throwable, RequestM[A]]): RequestM[A] = fa.recoverWith(pf)

      override def map[A, B](fa: RequestM[A])(f: A => B): RequestM[B] = fa.map(f)
      
      // NOTE: catchNonFatal() intentionally not given, because the default in ApplicativeError is correct.
      // This is *not* async, but that's probably correct -- RequestM favors synchronous, and this isn't a strong enough
      // motivation to require RequesterImplicits as an implicit argument for everything.
    }
}

object RequestMInstances extends RequestMInstances

private[cats] sealed trait RequestMInstances1 extends RequestMInstances2 {
  implicit def catsStdMonoidForRequestM[A: Monoid]: Monoid[RequestM[A]] =
    new RequestMMonoid[A]
}

private[cats] sealed trait RequestMInstances2 {
  implicit def catsStdSemigroupForRequestM[A: Semigroup]: Semigroup[RequestM[A]] =
    new RequestMSemigroup[A]
}

private[cats] abstract class RequestMCoflatMap extends CoflatMap[RequestM] {
  def map[A, B](fa: RequestM[A])(f: A => B): RequestM[B] = fa.map(f)
  def coflatMap[A, B](fa: RequestM[A])(f: RequestM[A] => B): RequestM[B] = RequestM.successful(f(fa))
}

private[cats] class RequestMSemigroup[A: Semigroup]
  extends ApplySemigroup[RequestM, A](RequestMInstances.catsStdInstancesForRequestM, implicitly)

private[cats] class RequestMMonoid[A](implicit A: Monoid[A])
  extends ApplicativeMonoid[RequestM, A](RequestMInstances.catsStdInstancesForRequestM, implicitly)