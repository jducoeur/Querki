package org.querki.requester

import scala.concurrent.Promise
import akka.actor._

/**
 * @author jducoeur
 */

object FailureTests {
  case class TestThrowable(th:Throwable)
  case class StartPromise(p:Promise[String])
  
  class PromiseActor extends QTestActor {
    def doReceive = {
      case StartPromise(p) => {
        
      }
    }
  }
}

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
