package org.querki.requester

import akka.actor._

/**
 * @author jducoeur
 */
object StackTests {
  case class Accumulate(nTerms:Int)
  
  /**
   * This works similarly to the doubling tests, but grows more slowly, so that we can
   * pressure the stack without hitting Int overflow first.
   */
  class Accumulator extends QTestActor {
    
    lazy val adder = context.actorOf(Props(classOf[Adder]))
    
    def doReceive = {
      case Accumulate(nTerms) => askAdder(nTerms, 0) foreach { result => sender ! result }
    }
    
    def askAdder(nTerms:Int, total:Int):RequestM[Int] = {
      if (nTerms == 0) {
        RequestM.successful(total)
      } else {
        adder.requestFor[Int](Add(nTerms, total)) flatMap { newTotal =>
          askAdder(nTerms - 1, newTotal)
        }
      }
    }
  }
  
  case class Add(x:Int, y:Int)
  
  class Adder extends QTestActor {
    def doReceive = {
      case Add(x, y) => sender ! x + y 
    }
  }
}

class StackTests extends RequesterTests {
  import StackTests._
  
  // Test very deeply nested flatMaps. Note that this test takes a relatively long time (~1 second) to run.
  "A deep stack of flatMaps" should {
    "not blow out the stack" in {
      val accum = system.actorOf(Props(classOf[Accumulator]))
      // The heart of the test: this had been causing a StackOverflow when set to ~2000 until we
      // added the non-stack-based unwinding mechanism:
      accum ! Accumulate(2000)
      expectMsg(2001000)
    }
  }
}
