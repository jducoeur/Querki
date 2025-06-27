package org.querki.requester

import scala.concurrent.Promise
import akka.actor._
import org.scalatest.{DoNotDiscover}

/**
 * @author jducoeur
 */

object FailureTests {
  case class TestThrowable(th: Throwable)
  case class StartPromise(p: Promise[String])

  class PromiseActor extends QTestActor {

    def doReceive = {
      case StartPromise(p) => {}
    }
  }
}

// TODO: the Requester tests are having problems in the integrated application, but for now we don't care.
// If/when Requester gets lifted back out to its own library, re-enable these tests and get them working right.
@DoNotDiscover
class FailureTests extends RequesterTests {
  import FailureTests._

  "A Requester" should {
    "be able to catch an Exception through a Future" in {
      val myPromise = Promise[String]
      val pActor = system.actorOf(Props(classOf[PromiseActor]))
      pActor ! StartPromise(myPromise)
    }
  }
}
