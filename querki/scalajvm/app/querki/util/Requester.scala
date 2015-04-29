package querki.util

import scala.concurrent.duration._
import scala.concurrent.{Future, Promise}
import scala.util.{Try,Success,Failure}
import scala.reflect.ClassTag

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
  
/**
 * The request "monad". It's actually a bit suspicious, in that it's mutable, but by and
 * large this behaves the way you expect a monad to work. In particular, it works with for
 * comprehensions, allowing you to compose requests much the way you normally do Futures.
 * 
 * @param promise If this request had an implicit Promise in scope, that Promise gets saved
 *   away here. This is mainly so that exceptions can properly fail the Promise. If there is
 *   no implicit Promise, this will be set to Requester.emptyPromise, which is simply a signal
 *   that there isn't really one.
 */
class RequestM[T](val promise:Promise[_]) {
  /**
   * The actions to take after this Request resolves.
   * 
   * Yes, this is mutable. That's arguably sad and evil, but remember that this is only intended
   * for use inside the pseudo-single-threaded world of an Actor.
   */
  private var callbacks = List.empty[Function[T, _]]
  
  private def propagateError(th:Throwable) = {
    if (promise eq Requester.emptyPromise) {
      // Do nothing -- we don't have a real promise to fulfill
      println(s"$this got error $th")
      // TBD: should we crash the requester?
    } else {
      // We've failed, so stop here and report the exception
      promise.failure(th)
    }    
  }
  
  private [util] def resolve(v:Try[T]):Unit = {
    v match {
      case Success(v) => {
        try {
          callbacks foreach { cb => cb(v) }
        } catch {
          case th:Throwable => propagateError(th)
        }
      }
      case Failure(ex) => propagateError(ex)
    }
  }
  
  def handle(handler:T => _):Unit = {
    callbacks :+= handler
  }
  
  def foreach(handler:T => Unit):Unit = {
    val wrappedHandler:Function[T,Unit] = { v =>
      handler(v)
    }
    handle(handler)
  }
  
  def map[U](handler:T => U):RequestM[U] = {
    val child:RequestM[U] = new RequestM(promise)
    val wrappedHandler:Function[T,U] = { v =>
      val result = handler(v)
      child.resolve(Success(result))
      result
    }
    handle(wrappedHandler)
    child
  }
  
  def flatMap[U](handler:T => RequestM[U]):RequestM[U] = {
    handle(handler)
    val child:RequestM[U] = new RequestM(promise)
    child
  }
  
  def filter(p:T => Boolean):RequestM[T] = {
    val filtered = new RequestM[T](promise)
    val filteringCb:Function[T,_] = { v:T =>
      if (p(v)) {
        filtered.resolve(Success(v))
      }
    }
    handle(filteringCb)
    filtered
  }
  
  def withFilter(p:T => Boolean):RequestM[T] = filter(p)
}

/**
 * Implicit that hooks into *other* Actors, to provide the nice request() syntax to send
 * messages to them. These implicits are available to any Actor that mixes in Requester, but
 * RequesterImplicits should also be mixed into any other class that wants access to this
 * capability. Those other classes must have access to a Requester -- usually, they should be
 * functional classes owned *by* a Requester.
 * 
 * This trait gives you the functions that you actually call directly -- request(), requestFor()
 * and requestFuture(). But those calls mostly create RequestM objects, and the actual work gets
 * done by the associated Requester.
 */
trait RequesterImplicits {
  
  /**
   * The actual Requester that is going to send the requests and process the responses. If
   * you mix RequesterImplicits into a non-Requester, this must point to that Actor, which
   * does all the real work. (If you are using this from within Requester, it's already set.)
   */
  def requester:Requester
    
  /**
   * This is a wrapper, intended to surround a request clause whose value you want to return
   * as a Future. There is no magic here -- it is simply syntactic sugar to make this pattern
   * easier. You should use it along these lines:
   * {{{
   * requestFuture[TargetType] { implicit promise =>
   *   for {
   *     v1 <- someActor.request(MyMessage)
   *     v2 <- anotherActor.request(AnotherMessage(v1))
   *   }
   *     promise.success(v2)
   * }
   * }}}
   * That is, requestFuture expects a block that takes a Promise of the type you want to return.
   * This block should declare that Promise as implicit -- that implicit Promise gets sucked into
   * the requests, and is used if an exception is thrown.
   * 
   * @param reqFunc An arbitrarily-complex request clause, which eventually calls promise.success()
   *   when it gets the desired answer.
   * @returns A Future of the specified type, which is hooked into the requests.
   */
  def requestFuture[R](reqFunc:Promise[R] => Any)(implicit tag: ClassTag[R]):Future[R] = {
    val promise = Promise[R]
    reqFunc(promise)
    promise.future
  }
  
  /**
   * Hook to add the request() methods to a third-party Actor.
   */
  implicit class RequestableActorRef(target:ActorRef) {
    /**
     * The basic, simple version of request() -- sends a message, process the response.
     * 
     * You can think of request as a better-behaved version of ask. Where ask sends a message to the
     * target actor, and gives you a Future that will execute when the response is received, request
     * does the same thing but will process the resulting RequestM '''in the Actor's receive loop'''.
     * While this doesn't save you from every possible problem, it makes it much easier to write clear
     * and complex operations, involving coordinating several different Actors, without violating the
     * central invariants of the Actor model.
     * 
     * The current sender will be preserved and will be active during the processing of the results,
     * so you can use it as normal.
     * 
     * This version of the call does not impose any expectations on the results. You can use a
     * destructuring case class in a for comprehension if you want just a single return type, or you
     * can map the RequestM to a PartialFunction in order to handle several possible returns.
     * 
     * @param msg The message to send to the target actor.
     * @param promise If there is an implicit Promise in scope, errors will cause that Promise to fail.
     */
    def request(msg:Any)(implicit promise:Promise[_] = Requester.emptyPromise):RequestM[Any] = {
      val req = new RequestM[Any](promise)
      requester.doRequest[Any](target, msg, req)
      req
    }
    
    /**
     * A more strongly-typed version of request().
     * 
     * This works pretty much exactly like request, but expects that the response will be of type T. It will
     * throw a ClassCastException if anything else is received. Otherwise, it is identical to request().
     */
    def requestFor[T](msg:Any)(implicit promise:Promise[_] = Requester.emptyPromise, tag: ClassTag[T]):RequestM[T] = {
      val req = new RequestM[T](promise)
      requester.doRequest[T](target, msg, req)
      req
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
 *     otherActor.request(MyRequest(b)).foreach {
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
  case class RequestedResponse[T](response:Try[T], handler:RequestM[T]) {
    def invoke = { handler.resolve(response) }
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
  def doRequest[T](otherActor:ActorRef, msg:Any, handler:RequestM[T])(implicit tag: ClassTag[T]) = {
    val originalSender = sender
    val f = otherActor ask msg
    import context.dispatcher
    val fTyped = f.mapTo[T]
    fTyped.onComplete {
      case Success(resp) => {
        try {
          self.tell(RequestedResponse(Success(resp), handler), originalSender)
        } catch {
          // TBD: is this ever going to plausibly happen?
          case ex:Exception => {
            self.tell(RequestedResponse(Failure(ex), handler), originalSender)
          }
        }
      }
      case Failure(thrown) => {
        self.tell(RequestedResponse(Failure(thrown), handler), originalSender)
      }
    }
  }
  
  def handleRequestResponse:Actor.Receive = {
    case resp:RequestedResponse[_] => resp.invoke
  }
}

object Requester {
  /**
   * This is the default Promise, used if there isn't an implicit one in scope. It signals that there is
   * *not* a Future being worked on, so we shouldn't try sticking Exceptions into it.
   */
  val emptyPromise = Promise[Unit]
}
