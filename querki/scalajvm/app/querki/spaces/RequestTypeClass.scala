package querki.spaces

import scala.util.{Failure, Success, Try}

import org.querki.requester._

/**
 * This is a typeclass that abstracts RequestM. We use it in SpaceCore instead of using RequestM
 * explicitly, so that we can replace it with synchronous operations in unit tests.
 * 
 * (Yes, this is a rather odd typeclass, since it is stateful. I'm still not clear how one builds
 * a pure typeclass that fits Scala's expected signatures for Monads.)
 * 
 * As of this writing, this is *wildly* experimental.
 */
trait RequestTC[A, F[A]] {
  def flatMap[B](f: A => F[B]):F[B]
  def map[B](f: A => B):F[B]
  def onComplete(f: PartialFunction[Try[A],Unit]):Unit
}

trait RTCAble[F[_]] {
  def toRTC[A](f:F[A]):RequestTC[A, F]
  def successful[A](a: A):F[A]
  def failed[T](ex:Exception):F[T]
}

object RequestTC {
  class RealRequestTC[A](rm:RequestM[A]) extends RequestTC[A, RequestM] {
    def flatMap[B](f: A => RequestM[B]) = rm.flatMap(f)
    def map[B](f: A => B) = rm.map(f)
    def onComplete(f: PartialFunction[Try[A],Unit]) = rm.onComplete(f)
  }
  
  /**
   * This is the critical handle that you need to specify explicitly -- this is essentially
   * a RequestM provider.
   */
  object RealRTCAble extends RTCAble[RequestM] {
    def toRTC[A](f:RequestM[A]) = new RealRequestTC(f)
    def successful[A](a: A) = RequestM.successful(a)
    def failed[T](ex:Exception) = RequestM.failed(ex)
  }
}

//
// The rest of this file probably belongs somewhere under test, and should actuall
// be played with.
//

object TestRequestTC {
  class TCIdentity[A](vt:Try[A]) {
    def flatMap[B](f: A => TCIdentity[B]):TCIdentity[B] = {
      vt match {
        case Success(v) => f(v)
        case Failure(ex) => new TCIdentity(Failure[B](ex))
      }
    }
    def map[B](f: A => B):TCIdentity[B] = {
      vt match {
        case Success(v) => new TCIdentity(Success(f(v)))
        case Failure(ex) => new TCIdentity(Failure[B](ex))
      }
    }
    def onComplete(f: PartialFunction[Try[A],Unit]) = f(vt)
  }
  
  class TestRequestTC[A](rm:TCIdentity[A]) extends RequestTC[A, TCIdentity] {
    def flatMap[B](f: A => TCIdentity[B]) = rm.flatMap(f)
    def map[B](f: A => B) = rm.map(f)
    def onComplete(f: PartialFunction[Try[A],Unit]) = rm.onComplete(f)    
  }
  
  object TestRTCAble extends RTCAble[TCIdentity] {
    def toRTC[A](f:TCIdentity[A]) = new TestRequestTC(f)
    def successful[A](a: A) = new TCIdentity(Success(a))
    def failed[T](ex:Exception) = new TCIdentity(Failure(ex))
  }
}

abstract class RTCTest[RM[_]](rtc:RTCAble[RM]) {
  implicit def rm2rtc[A](rm:RM[A]) = rtc.toRTC(rm)
  
  def something(foo:String):RM[Int]
  
  def thingy(blah:RM[String]):RM[Int] = {
    for {
      baz <- blah
      bar <- something(baz)
    }
      yield bar
  }
}

object RTCTest2 {
  
  class RequestMTest extends RTCTest[RequestM](RequestTC.RealRTCAble)
  
  val rtcTest = new RequestMTest
  val x = rtcTest.thingy(RequestM.successful("hello"))
  
}