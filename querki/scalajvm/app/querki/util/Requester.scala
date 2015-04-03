package querki.util

import scala.util.{Success,Failure}

import scala.concurrent.duration._
import scala.concurrent.{Future, Promise}

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout

/**
 * Implicit that hooks into *other* Actors, to provide the nice request() syntax to send
 * messages to them. These implicits are available to any Actor that mixes in Requester, but
 * RequesterImplicits should also be mixed into any other class that wants access to this
 * capability. Those other classes must have access to a Requester -- usually, they should be
 * functional classes owned *by* a Requester.
 */
trait RequesterImplicits {
  
  /**
   * The actual Requester that is going to send the requests and process the responses. If
   * you mix RequesterImplicits into a non-Requester, this must point to that Actor, which
   * does all the real work.
   */
  def requester:Requester
  
  /**
   * Hook to add the request() methods to a third-party Actor.
   */
  implicit class RequestableActorRef(target:ActorRef) {
    /**
     * The basic, simple version of request() -- sends a message, process the response.
     * 
     * This version doesn't return anything, and is intended for use inside normal Actors.
     * Usually, handler will pass messages back to other Actors, or simply alter the state
     * of this one.
     * 
     * @param msg The message to send to the target actor.
     * @param handler A standard Receive handler, which deals with all of the known responses.
     */
    def request(msg:Any)(handler:Actor.Receive) = requester.doRequest(target, msg)(handler)
    
    /**
     * A request mechanism that returns a Future of the *handled* response. That is, this should
     * be used when you want to get a response, massage it, and pass along that massaged result.
     * 
     * Use this with care -- the returned Future should not usually be processed much in the context
     * of the Actor, for the usual reason that Futures inside Actors are problematic. But it is a nice
     * concise way to pass along results to, eg, Autowire, to send those results back to the client.
     * 
     * @param msg The message to send to the target Actor
     * @param handler A function that deals with the response from the target and massages it
     *   as desired. This returns a value of type T, or throws an exception if something goes wrong.
     * @tparam T The type that should be returned by handler.
     */
    def requestFor[T](msg:Any)(handler:PartialFunction[Any,T]):Future[T] = {
      val promise = Promise[T]
      val wrappedHandler:Actor.Receive = PartialFunction({ response:Any =>
        try {
          val result = handler(response)
          promise.success(result)
        } catch {
          case th:Throwable => promise.failure(th)
        }
      })
      requester.doRequest(target, msg)(wrappedHandler)
      promise.future
    }
    
    /**
     * The outer layer for nested Requests that return Futures. Think of this as flatMap, where
     * requestFor is map.
     * 
     * TODO: there is a monadic handler, usable with for expressions, desperately trying to break out here...
     */
    def requestNested[T](msg:Any)(handler:PartialFunction[Any,Future[T]]):Future[T] = {
      val promise = Promise[T]
      val wrappedHandler:Actor.Receive = PartialFunction({ response:Any =>
        try {
          val result = handler(response)
          promise.completeWith(result)
        } catch {
          case th:Throwable => promise.failure(th)
        }
      })
      requester.doRequest(target, msg)(wrappedHandler)
      promise.future
    }
  }

}

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
trait Requester extends Actor with RequesterImplicits {
  
  val requester = this
  
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
  
  override def unhandled(message: Any): Unit = {
    message match {
      case resp:RequestedResponse => resp.invoke
      case other => super.unhandled(other)
    }
  }
}