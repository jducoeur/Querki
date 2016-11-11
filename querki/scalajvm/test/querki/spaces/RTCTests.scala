package querki.spaces

import scala.util.{Failure, Success, Try}

import querki.test._

private [spaces] class NotYetResolvedException extends Exception

class TCIdentity[A](vtIn:Try[A]) {
  
  var vt = vtIn
  
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
  def filter(p:A => Boolean):TCIdentity[A] = {
    vt match {
      case Success(v) => {
        if (p(v))
          this
        else
          new TCIdentity(Failure[A](new Exception(s"Filtered out a TCIdentity! Is that a bug?")))
      }
      case Failure(ex) => this
    }
  }
  def onComplete(f: PartialFunction[Try[A],Unit]) = f(vt)
  def resolve(v:Try[A]):Unit = { vt = v }
}

class TestRequestTC[A](rm:TCIdentity[A]) extends RequestTC[A, TCIdentity] {
  def flatMap[B](f: A => TCIdentity[B]) = rm.flatMap(f)
  def map[B](f: A => B) = rm.map(f)
  def filter(p:A => Boolean) = rm.filter(p)
  def onComplete(f: PartialFunction[Try[A],Unit]) = rm.onComplete(f)
  def resolve(v:Try[A]):Unit = rm.resolve(v)
}

/**
 * This is the main hook to the "test" version of RequestTC. Whereas the real one is
 * a layer over RequestM, this is just a simple identity that holds either a value or
 * an exception, and runs synchronously.
 */
object TestRTCAble extends RTCAble[TCIdentity] {
  def toRTC[A](f:TCIdentity[A]) = new TestRequestTC(f)
  def successful[A](a: A) = new TCIdentity(Success(a))
  def failed[T](ex:Exception) = new TCIdentity(Failure(ex))
  def prep[T] = new TCIdentity(Failure(new NotYetResolvedException))
}

/**
 * Tests for the RequestTC and related stuff.
 */
class RTCTests extends QuerkiTests {

  /**
   * This is a trivial little class that just provides some monadic usage, to make
   * sure things work as expected.
   * 
   * Note that the RM type here is abstract -- we just know that it's a wrapper type
   * that can be obtained and manipulated by RTCAble. *Structurally*, it's a RequestM,
   * and testing that is the point of the exercise.
   */
  abstract class RTCTest[RM[_]](rtc:RTCAble[RM]) {
    /**
     * Given an RM from some operation, convert it to the prevailing RequestTC type so
     * we can work with it.
     */
    implicit def rm2rtc[A](rm:RM[A]) = rtc.toRTC(rm)
    
    def double(foo:Int):RM[Int] = {
      if (foo < 0)
        rtc.failed(new Exception("I'm too stupid to multiply negative numbers!"))
      else
        rtc.successful(foo * 2)
    }
    def something(foo:Int):RM[String] = rtc.successful(foo.toString)
    
    def doubledString(blah:RM[Int]):RM[String] = {
      for {
        originalNum <- blah
        doubled <- double(originalNum)
        stringified <- something(doubled)
      }
        yield stringified
    }
  }
  
  "TCIdentity" should {
    class TCITest extends RTCTest[TCIdentity](TestRTCAble)
    val tester = new TCITest
      
    "work in the success case" in {
      tester.doubledString(TestRTCAble.successful(12)).onComplete { 
        case Success(result) => result should equal ("24")
        case Failure(ex) => throw ex
      }
    }
    
    "work in the failure case" in {
      tester.doubledString(TestRTCAble.successful(-1)).onComplete { 
        case Success(result) => throw new Exception(s"Unexpected success from TCITest: got $result")
        case Failure(ex) => // This is what we expect
      }      
    }
  }
}
