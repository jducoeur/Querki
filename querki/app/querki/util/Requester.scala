package querki.util

import scala.util.{Success,Failure}

import scala.concurrent.duration._
import scala.concurrent.Future

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout

/**
 * Easy and relatively safe variant of "ask".
 * 
 * The idea here is that it would be lovely to have a replacement for the "ask" pattern. Ask
 * is powerful, but quite dangerous -- in particular, handling the response in the most obvious
 * way, using the Future's completion callbacks, is a fine way to break your Actor's threading
 * and cause strange timing bugs.
 * 
 * So the idea here is to build something with similar semantics to ask, but deliberately a bit
 * dumbed-down and safer for routine use. Where ask returns a Future that you can then put 
 * callbacks on, request() takes those callbacks as parameters, and runs them *in this Actor's main thread*.
 * 
 * In other words, I want to be able to say something like:
 * 
 * def receive = {
 *   ...
 *   case MyMessage(a, b) => {
 *     otherActor.request(MyRequest(b)) {
 *       case OtherResponse(c) => ...
 *     }
 *   }
 * }
 * 
 * While OtherResponse is lexically part of MyRequest, it actually *runs* during receive, just like
 * any other incoming message, so it isn't prone to the threading problems that ask is.
 * 
 * Note that point-free style does *not* work, unfortunately -- because request has multiple parameter
 * lists, my attempts to use this point-free get confused about what the block belongs to. I don't know
 * if there's a way to fix that.
 * 
 * How does this work? Under the hood, it actually does use ask, but in a very specific and constrained
 * way. We send the message off using ask, and then hook the resulting Future. When the Future completes,
 * we wrap the response and the handler together in a RequestedResponse message, and loop that back
 * around as a message to the local Actor. 
 * 
 * Note that the original sender is preserved, so the callback can use it without problems. (This is the
 * most common error made when using ask, and was one of the motivations for creating Requester.) 
 * 
 * Note that, to make this work, the Request trait mixes in its own version of unhandled(). I *think* this
 * should Just Work, but it's probably the part where I'm on least-comfortable ground, so watch for edge
 * cases there. I have not yet tested how this interacts with Actor-defined unhandled(), and I'm mildly
 * concerned about possible loops.
 * 
 * IMPORTANT: Requester is *not* compatible with stateful versions of become() -- that is, if you are
 * using become() in a method where you are capturing the parameters in the closure of become(),
 * Requester will probably not work right. This is because the body of the response handler will capture
 * the closed-over parameter, and if the Actor has become() something else in the meantime, the handler
 * will use the *old* data, not the new.
 * 
 * More generally, Requester should be used with great caution if your Actor changes state frequently.
 * While it *can* theoretically be used with FSM, it may not be wise to do so, since the state machine
 * may no longer be in a compatible state by the time the response is received. Requester is mainly intended
 * for Actors that spend most or all of their time in a single state; it generally works quite well for those.
 */
trait Requester { me:Actor =>
  
  implicit class RequestableActorRef(a:ActorRef) {
    def request(msg:Any)(handler:Actor.Receive) = doRequest(a, msg)(handler)
  }
  
  /**
   * The response from request() will be wrapped up in here and looped around. You shouldn't need to
   * use this directly. 
   */
  case class RequestedResponse(response:Any, handler:Actor.Receive) {
    def invoke = handler(response)
  }
  
  /**
   * Override this to specify the timeout for requests
   */
  implicit val requestTimeout = Timeout(10 seconds)
 
  /**
   * Send a request, and specify the handler for the received response. You may also specify a failHandler,
   * which will be run if the operation fails for some reason. (Most often, because we didn't receive a
   * response within the timeout window.)
   */
  def doRequest(otherActor:ActorRef, msg:Any)(handler:Actor.Receive) = {
    val originalSender = sender
    val f = otherActor ask msg
    import context.dispatcher
    f.onComplete {
      case Success(resp) => self.tell(RequestedResponse(resp, handler), originalSender) 
      case Failure(thrown) => self.tell(RequestedResponse(thrown, handler), originalSender) 
    }
  }
  
  abstract override def unhandled(message: Any): Unit = {
    message match {
      case resp:RequestedResponse => resp.invoke
      case other => me.unhandled(other)
    }
  }
}