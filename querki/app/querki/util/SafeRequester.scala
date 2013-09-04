package querki.util

import scala.util.{Success,Failure}

import scala.concurrent.duration._
import scala.concurrent.Future

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout

/**
 * PROTOTYPE IMPLEMENTATION OF REQUESTER.
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
 *     otherActor request MyRequest(b) {
 *       case OtherResponse(c) => ...
 *     }
 *   }
 * }
 * 
 * While OtherResponse is lexically part of MyRequest, it actually *runs* during receive, just like
 * any other incoming message, so it isn't prone to the threading problems that ask is.
 * 
 * How does this work? Under the hood, it actually does use ask, but in a very specific and constrained
 * way. We send the message off using ask, and then hook the resulting Future. When the Future completes,
 * we wrap the response and the handler together in a RequestedResponse message, and loop that back
 * around as a message to the local Actor.
 * 
 * Unfortunately, this does demand a bit of extra complicity on the Actor's part. In order to use this,
 * you must mix in this trait and add one clause to your receive:
 * 
 * def receive = handleResponses orElse {
 *   ... normal receive handlers...
 * }
 * 
 * handleResponse simply receives the RequestedReponse message, and calls invoke on it when received.
 * You can handle this in other ways if you prefer, but this seems cleanest. (I wish I could see a way
 * to avoid that detail, but so far I don't see one that is fully compatible with idiomatic Akka. And part
 * of the goal here is that this mechanism can be used without doing too much violence to the rest of your Akka code.)
 */
trait Requester { this:Actor =>
  
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
  implicit val requestTimeout = Timeout(1 seconds)
  
  def handleResponses:Receive = {
    case resp:RequestedResponse => resp.invoke
  }
 
  /**
   * Send a request, and specify the handler for the received response. You may also specify a failHandler,
   * which will be run if the operation fails for some reason. (Most often, because we didn't receive a
   * response within the timeout window.)
   */
  def doRequest(otherActor:ActorRef, msg:Any)(handler:Actor.Receive)(failHandler:Option[Actor.Receive]) = {
    val f = otherActor ask msg
    import context.dispatcher
    f.onComplete {
      case Success(resp) => self ! RequestedResponse(resp, handler) 
      case Failure(thrown) => failHandler.map(h => self ! RequestedResponse(thrown, h))
    }
  }
  
  def request(otherActor:ActorRef, msg:Any)(handler:Actor.Receive) = {
    doRequest(otherActor, msg)(handler)(None)
  }
  
  def requestOrFail(otherActor:ActorRef, msg:Any)(handler:Actor.Receive)(failHandler:Actor.Receive) = {
    doRequest(otherActor, msg)(handler)(Some(failHandler))
  }
}