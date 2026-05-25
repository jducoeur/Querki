package org.querki.requester

import scala.util.{Failure, Success}
import akka.actor._
import akka.util.Timeout

import scala.concurrent.duration._
import akka.pattern.AskTimeoutException

object RetryTests {
  case object Start
  case class DropFirstN(n: Int)
  case object GaveUp

  /**
   * A variant of Doubler that simply drops the first N requests on the floor.
   */
  class FinickyDoubler extends Actor {
    var nToDrop: Int = 0

    def receive = {
      case DropFirstN(n) => nToDrop = n

      case n: Int => {
        if (nToDrop > 0)
          nToDrop -= 1
        else
          sender ! n * 2
      }
    }
  }

  class Tester(
    n: Int,
    drops: Int,
    retries: Int
  ) extends Actor
       with Requester {

    lazy val doubler = context.actorOf(Props(classOf[FinickyDoubler]))

    override implicit val requestTimeout: Timeout = Timeout(1.millis)

    def receive = {
      case Start => {
        doubler ! DropFirstN(drops)
        val req = for {
          result <- doubler.request(n, retries)
        } yield result

        req.onComplete {
          case Success(result: Int) => {
            sender ! result
            context.stop(doubler)
            context.stop(self)
          }

          case Failure(ex: AskTimeoutException) => {
            sender ! GaveUp
            context.stop(doubler)
            context.stop(self)
          }

          case other => throw new Exception(s"Got unexpected response $other")
        }
      }
    }
  }
}

/**
 * These tests exercise request's retry mechanism. The recipient Actor drops some messages on the
 * floor, so that they have to be retried; we test retrying, as well as retry failures.
 */
//class RetryTests extends RequesterTests {
//  import RetryTests._
//
//  def startTest(
//    n: Int,
//    drops: Int,
//    retries: Int
//  ) = {
//    val tester = system.actorOf(Props(classOf[Tester], n, drops, retries))
//    tester ! Start
//  }
//
//  // TODO: these tests are flapping when running the full test suite. Rewrite them to be properly deterministic,
//  // then re-enable:
//  "Requester with retries" should {
//    "work normally if nothing goes wrong" in {
//      startTest(3, 0, 0)
//      expectMsg(6)
//    }
//
//    "retry successfully once" in {
//      startTest(3, 1, 3)
//      expectMsg(6)
//    }
//
//    "retry successfully n times" in {
//      startTest(3, 3, 3)
//      expectMsg(6)
//    }
//
//    "fail after n+1 tries" in {
//      startTest(3, 4, 3)
//      expectMsg(GaveUp)
//    }
//
//    "fail if retry isn't attempted" in {
//      startTest(3, 1, 0)
//      expectMsg(GaveUp)
//    }
//  }
//}
