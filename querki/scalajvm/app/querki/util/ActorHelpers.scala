package querki.util

import scala.concurrent.Await
import scala.concurrent.duration._

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout

/**
 * Little utility helpers for Akka.
 */
object ActorHelpers {
  
  def timeout = Timeout(Config.getDuration("querki.akka.timeout"))
  
  /**
   * This is basically a pimped ActorRef with some additional utils.
   */
  implicit class QuerkiActorRef(ref:ActorRef) {
    /**
     * TODO: this really isn't correct. How can we pick up the implicit Timeout from the
     * caller?
     */
    implicit val timeout = ActorHelpers.timeout

    /**
     * HACK: wrapper around ask, for code that is in the process of evolution.
     * 
     * All uses of askBlocking should be considered bad smells, and that is the reason to
     * use it. It should be used as the midpoint when converting old synchronous code to
     * Actor-based asynchronous code.
     */
    def askBlocking[R](msg:Any)(handler:PartialFunction[Any, R]):R = {
      val fut = ref ? msg
      val intermediate = Await.result(fut, (5 seconds))
      handler(intermediate)
    }
  }
}