package org.querki.requester

import scala.concurrent.Future
import akka.actor._
import org.scalatest.concurrent._

/**
 * @author jducoeur
 */
object FutureTests {
  case object Start
  case class TestFuture(f:Future[Int])
  
  class NewFutureActor(error:Boolean) extends QTestActor {
    def doReceive = {
      case Start => {
        val f:Future[Int] =
          for {
            four <- doubler.requestFor[Int](2)
            eight <- doubler.requestFor[Int](four)
            dummy = if (error) throw new Exception("BOOM")
            sixteen <- doubler.requestFor[Int](eight)
          }
            yield sixteen
        
        sender ! TestFuture(f)
      }
    }
  }
}

class FutureTests extends RequesterTests with Futures with ScalaFutures {
  import FutureTests._
  
  "Requester" should {
    "be able to work through a Future, new-style" in {
      val actor = system.actorOf(Props(classOf[NewFutureActor], false))
      actor ! Start
      val TestFuture(fut) = receiveOne(dur)
      whenReady(fut) { n =>
        assert(n == 16)
      }
    }
    
    "be able to catch an Exception through a Future, new-style" in {
      val actor = system.actorOf(Props(classOf[NewFutureActor], true))
      actor ! Start
      val TestFuture(fut) = receiveOne(dur)
      whenReady(fut.failed) { ex =>
        assert(ex.getMessage == "BOOM")
      }
    }
  }
}