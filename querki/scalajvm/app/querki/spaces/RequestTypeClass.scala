package querki.spaces

import scala.util.{Failure, Success, Try}

import org.querki.requester._

/**
 * This is a typeclass that abstracts RequestM. We use it in SpaceCore instead of using RequestM
 * explicitly, so that we can replace it with synchronous operations in unit tests.
 * 
 * Yes, this is a slightly odd typeclass, since it is stateful. I'm still not clear how one builds
 * a pure typeclass that fits Scala's expected signatures for Monads, although I gather that
 * scalaz has some magic that might do the trick. In practice, for now RTCAble is the "real" typeclass,
 * used to generate instances of RequestTC.
 * 
 * As of this writing, this is *wildly* experimental. If it works well, we might promote it into
 * the Requester library itself, and flesh out the rest of the Requester signatures. Really, though, at
 * that point we could do it more cheaply and easily by defining a *base* trait of Requester and
 * an providing alternate implementation. This is to a large degree a proof of concept of how to do
 * this sort of thing *without* altering the class.
 */
trait RequestTC[A, F[A]] {
  def flatMap[B](f: A => F[B]):F[B]
  def map[B](f: A => B):F[B]
  def onComplete(f: PartialFunction[Try[A],Unit]):Unit
  def resolve(v:Try[A]):Unit
}

trait RTCAble[F[_]] {
  def toRTC[A](f:F[A]):RequestTC[A, F]
  def successful[A](a: A):F[A]
  def failed[T](ex:Exception):F[T]
  def prep[T]:F[T]
}

/**
 * The RequestTC that wraps around an actual RequestM.
 * 
 * This *should* be a Value Class, and I wish it was; unfortunately, I don't believe RequestTC
 * counts as a universal trait.
 */
class RealRequestTC[A](rm:RequestM[A]) extends RequestTC[A, RequestM] {
  def flatMap[B](f: A => RequestM[B]) = rm.flatMap(f)
  def map[B](f: A => B) = rm.map(f)
  def onComplete(f: PartialFunction[Try[A],Unit]) = rm.onComplete(f)
  def resolve(v:Try[A]):Unit = rm.resolve(v)
}

/**
 * This is the critical handle that you need to specify explicitly -- this is essentially
 * a RequestM provider.
 */
object RealRTCAble extends RTCAble[RequestM] {
  def toRTC[A](f:RequestM[A]) = new RealRequestTC(f)
  def successful[A](a: A) = RequestM.successful(a)
  def failed[T](ex:Exception) = RequestM.failed(ex)
  def prep[T] = RequestM.prep[T]()
}
