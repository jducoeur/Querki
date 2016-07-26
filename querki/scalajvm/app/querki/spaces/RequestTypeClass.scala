package querki.spaces

import scala.util.Try

import org.querki.requester._

/**
 * This is a typeclass that abstracts RequestM. We use it in SpaceCore instead of using RequestM
 * explicitly, so that we can replace it with synchronous operations in unit tests. 
 * 
 * As of this writing, this is *wildly* experimental.
 */
trait RequestTC[A, F[A]] {
  def flatMap[B](f: A => F[B]):F[B]
  def map[B](f: A => B):F[B]
  def onComplete[A](f: PartialFunction[Try[A],Unit]):Unit
}

trait RTCAble[F[_]] {
  def toRTC[A](f:F[A]):RequestTC[A, F]
  def successful[A](a: A):F[A]
}

object RequestTC {
  class RealRequestTC[A](rm:RequestM[A]) extends RequestTC[A, RequestM] {
    def flatMap[B](f: A => RequestM[B]) = rm.flatMap(f)
    def map[B](f: A => B) = rm.map(f)
    def onComplete(f: PartialFunction[Try[A],Unit]) = rm.onComplete(f)
  }
  
  object RealRTCAble extends RTCAble[RequestM] {
    def toRTC[A](f:RequestM[A]) = new RealRequestTC(f)
    def successful[A](a: A) = RequestM.successful(a)
  }
}

trait RTCTest {
  def something[RM[_]](foo:String):RM[Int]
  
  def thingy[A, RM[A]](blah:RM[String])(implicit rtc:RTCAble[RM]):RM[Int] = {
    implicit def rm2rtc[A](rm:RM[A]) = rtc.toRTC(rm)
    
    for {
      baz <- blah
      bar <- something[RM](baz)
    }
      yield bar
  }
  
  val x = thingy(RequestM.successful("hello"))(RequestTC.RealRTCAble)
}
