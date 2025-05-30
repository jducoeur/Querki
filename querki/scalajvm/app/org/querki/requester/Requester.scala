package org.querki.requester

import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.concurrent.{Future, Promise}
import scala.util.{Try,Success,Failure}
import scala.reflect.ClassTag

import akka.actor._
import akka.pattern.{ask, AskTimeoutException}
import akka.util.Timeout
  
/**
 * The request "monad". It's actually a bit nasty, in that it's mutable, but by and
 * large this behaves the way you expect a monad to work. In particular, it works with for
 * comprehensions, allowing you to compose requests much the way you normally do Futures.
 * But since it is mutable, it should never be used outside the context of an Actor.
 */
class RequestM[T](val enclosing:sourcecode.FullName, val method:sourcecode.Name, val file:sourcecode.File, val line:sourcecode.Line)
{
  /**
   * The actions to take after this Request resolves.
   * 
   * Yes, this is mutable. That's arguably sad and evil, but remember that this is only intended
   * for use inside the pseudo-single-threaded world of an Actor.
   */
  private var callbacks = List.empty[Function[Try[T], _]]
  
  /**
   * Store the result, so that if someone hooks a callback into me after I'm resolved, I
   * can handle that immediately. (This can happen if, for example, RequestM.successful()
   * comes into play.)
   * 
   * Yes, also mutable. Same argument. Deal.
   */
  private var result:Option[Try[T]] = None
  
  /**
   * Build up a chain of RequestM's, from lower levels to higher, so that we can unwind results
   * iteratively instead of through the callbacks.
   * 
   * This is rather ugly, but necessary. The original design did the unwinding completely cleanly,
   * with each inner RequestM propagating its result by calling resolve() on the level above it.
   * But that turned out to cause stack overflow crashes when you got to ~2000 levels of flatMap().
   * So we need a mechanism that can be handled iteratively (well, tail-recursively) instead.
   */
  protected var higherLevel:Option[RequestM[T]] = None
  protected def setHigherLevel(higher:RequestM[T]) = {
    // Note that sometimes this gets called *after* it has already resolved. That will typically
    // happen on the innermost flatMap(), which contains a synchronous map() that produces the
    // end result. So we need to detect that and trigger unwinding when it happens.
    result match {
      case Some(res) => {
        setFailureStack(res)
        unwindHigherLevels(res, Some(higher))
      }
      case None => higherLevel = Some(higher)
    }
  }
  @tailrec
  private def unwindHigherLevels(v:Try[T], currentLevel:Option[RequestM[T]]):Unit = {
    currentLevel match {
      case Some(higher) => {
        // Resolve the outer layers, but do *not* start more unwinding. This is key:
        higher.resolve(v, false)
        unwindHigherLevels(v, higher.higherLevel)
      }
      case None => // We're done unwinding, or didn't need to do it at all
    }
  }
  
  @tailrec 
  private def buildFailureStackRec(rm:RequestM[_], stack:List[StackTraceElement]):List[StackTraceElement] = {
    val withMe = new StackTraceElement(rm.enclosing.value, rm.method.value, rm.file.value, rm.line.value) :: stack
    rm.higherLevel match {
      case Some(higher) => buildFailureStackRec(higher, withMe)
      case None => withMe
    }
  }
  private def setFailureStack(t:Try[T]):Unit = {
    t match {
      case Success(_) =>
      case Failure(ex) => {
        val failStack = buildFailureStackRec(this, List.empty).reverse.toVector
        val originalStack = ex.getStackTrace.toVector
        val fullStack = originalStack ++ failStack
        ex.setStackTrace(fullStack.toArray)
      }
    }
  }
  
  private [requester] def intercept(test:PartialFunction[Try[T], Boolean]) = _blocker = Some(test)
  private var _blocker:Option[PartialFunction[Try[T], Boolean]] = None
  
  /**
   * Set the value for this RequestM, and run through its dependency chain.
   * 
   * You should *not* normally call this yourself -- it is mainly for internal use. Call this only
   * when you have obtained a RequestM through prep(), and need to finish it up, usually because
   * you have to work around some other concurrency construct. (Such as PersistentActor's persist()
   * call.)
   * 
   * You should ignore the startUnwind flag, which is for internal use only.
   */
  def resolve(v:Try[T], startUnwind:Boolean = true):Unit = {
    // We give the outside world a chance to prevent this from going through, in order to let the
    // retry system work.
    // TODO: this is ugly, and shouldn't be necessary if the functional interface was sufficiently
    // rich. Think about how to make retries work differently.
    val blocked = _blocker match {
      case Some(blocker) => blocker(v)
      case None => false
    }
    if (!blocked) {
      result = Some(v)
      try {
        if (startUnwind) {
          setFailureStack(v)
        }
        callbacks foreach { cb => cb(v) }
        if (startUnwind) {
          unwindHigherLevels(v, higherLevel)
        }
      } catch {
        case th:Throwable => {
          // TODO: Is there anything useful we can/should do to propagate this error?
          println(s"Exception while resolving Request: ${th.getMessage}")
          th.printStackTrace()
        }
      }
    }
  }
  
  def onComplete[U](handler: (Try[T]) => U):Unit = {
    result match {
      case Some(v) => handler(v)
      case None => callbacks :+= handler
    }
  }
  
  // TODO: This should probably become onSuccess
  def handleSucc(handler:T => _):Unit = {
    onComplete { t:Try[T] =>
      t match {
        case Success(v) => handler(v)
        case Failure(ex) =>
      }
    }
  }
  
  def foreach(handler:T => Unit):Unit = {
    handleSucc(handler)
  }
  
  def map[U](handler:T => U)(implicit enclosing:sourcecode.FullName, file:sourcecode.File, line:sourcecode.Line):RequestM[U] = {
    // What's going on here? I need to synchronously return a new RequestM, but I won't
    // actually complete until sometime later. So when I *do* complete, pipe that result
    // into the given handler function, and use that to resolve the returned child.
    val child:RequestM[U] = new RequestM(enclosing, "map", file, line)
    onComplete {
      case Success(v) => {
        try {
          val result = handler(v)
          child.resolve(Success(result))
          result
        } catch {
          case th:Throwable => child.resolve(Failure(th))
        }        
      }
      case Failure(ex) => child.resolve(Failure(ex))
    }
    child
  }
  
  /**
   * The central flatMap() operation, which as in any Monad is key to composing these
   * things together.
   * 
   * flatMap() is a bit tricky. The problem we have is that we need to return a RequestM
   * *synchronously* from flatMap, so that higher-level code can compose on it. But
   * the *real* RequestM being returned from handler won't come into existence until
   * some indefinite time in the future. So we need to create a new one right now,
   * and when the real one comes into existence, link its success to that of the one
   * we're returning here.
   * 
   * The initial version of this was beautiful, elegant, and caused stack overflows if
   * you nested flatMaps more than a couple thousand levels deep. (Which, yes, we occasionally do at
   * Querki.) The issue comes during "unwinding" time, when the innermost RequestM finally gets
   * set to a value. The original version had it then call resolve() on the one that contained it,
   * which called resolve() on its parent, and so on, until we finally blew the stack.
   * 
   * So instead, flatMap builds an ugly but practical linked list of RequestM's, with each one
   * essentially pointing to the one above it. We still call resolve() at each level, but those
   * are *not* recursive; instead, we walk up the flatMap chain *tail*-recursively, resolving each node
   * along the way. It's a bit less elegant, but doesn't cause the JVM to have conniptions. 
   */
  def flatMap[U](handler:T => RequestM[U])(implicit enclosing:sourcecode.FullName, file:sourcecode.File, line:sourcecode.Line):RequestM[U] = {
    val child:RequestM[U] = new RequestM(enclosing, "flatMap", file, line)
    onComplete {
      case Success(v) => {
        try {
          val subHandler = handler(v)
          // Note that flatMap specifically does *not* resolve this through onComplete any more.
          // The commented-out line worked well, and was nicely elegant, but resulted in stack
          // overflows. So instead we build an explicit chain, and unwind that way.
          subHandler.setHigherLevel(child)
//          subHandler.onComplete { u:Try[U] => child.resolve(u) }
          subHandler
        } catch {
          case th:Throwable => { child.resolve(Failure(th)) }
        }
      }
      case Failure(ex) => child.resolve(Failure(ex))
    }
    child
  }
  
  def filter(p:T => Boolean)(implicit enclosing:sourcecode.FullName, file:sourcecode.File, line:sourcecode.Line):RequestM[T] = {
    val filtered = new RequestM[T](enclosing, "filter", file, line)
    val filteringCb:Function[T,_] = { v:T =>
      if (p(v)) {
        filtered.resolve(Success(v))
      }
    }
    handleSucc(filteringCb)
    filtered
  }
  
  def withFilter(p:T => Boolean):RequestM[T] = filter(p)
  
  def recover[U >: T](pf: PartialFunction[Throwable, U]): RequestM[U] = {
    val child:RequestM[U] = new RequestM(enclosing, "recover", file, line)
    onComplete {
      case Success(v) => {
        try {
          child.resolve(Success(v))
        } catch {
          case th:Throwable => child.resolve(Success(pf(th)))
        }        
      }
      case Failure(ex) => child.resolve(Success(pf(ex)))
    }
    child
  }
  
  def recoverWith[U >: T](pf: PartialFunction[Throwable, RequestM[U]]): RequestM[U] = {
    val child:RequestM[U] = new RequestM(enclosing, "recoverWith", file, line)
    onComplete {
      case Success(v) => {
        try {
          child.resolve(Success(v))
        } catch {
          case th:Throwable => {
            pf(th) onComplete {
              case any => child.resolve(any)
            }
          }
        }        
      }
      case Failure(th) => {
        pf(th) onComplete {
          case any => child.resolve(any)
        }
      }
    }
    child    
  }
}

object RequestM {
  def successful[T](result:T)(implicit enclosing:sourcecode.FullName, file:sourcecode.File, line:sourcecode.Line):RequestM[T] = {
    val r = new RequestM[T](enclosing, "successful", file, line)
    r.resolve(Success(result))
    r
  }
  
  def failed[T](ex:Throwable)(implicit enclosing:sourcecode.FullName, file:sourcecode.File, line:sourcecode.Line):RequestM[T] = {
    val r = new RequestM[T](enclosing, "failed", file, line)
    r.resolve(Failure(ex))
    r
  }
  
  /**
   * This returns a newly-created RequestM, outside of the usual Requester pathways. This
   * is slightly dangerous, but useful -- think of it as the counterpart to creating a Promise
   * and returning its .future. The difference here is that we're using the same object for
   * both sides -- you can pass the returned RequestM around, map over it, and so on, and
   * resolve it at the appropriate time.
   * 
   * If you use prep/resolve, it is your responsibility to call resolve only at a safe time,
   * inside the Actor's receive loop. (Or inside persist() in a PersistentActor, or some such.)
   * Do this only when necessary; normally, you should work through .request().
   */
  def prep[T]()(implicit enclosing:sourcecode.FullName, file:sourcecode.File, line:sourcecode.Line):RequestM[T] = {
    new RequestM[T](enclosing, "prep", file, line)
  }
}

/**
 * Implicit that hooks into *other* Actors, to provide the nice request() syntax to send
 * messages to them. These implicits are available to any Actor that mixes in Requester, but
 * RequesterImplicits should also be mixed into any other class that wants access to this
 * capability. Those other classes must have access to a Requester -- usually, they should be
 * functional classes owned *by* a Requester.
 * 
 * This trait gives you the functions that you actually call directly -- request() and requestFor().
 * But those calls mostly create RequestM objects, and the actual work gets
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
     * @param retries The number of times to retry this request, if it times out.
     */
    def request(msg:Any, retries:Int = 0):RequestM[Any] = {
      requestFor[Any](msg, retries)
    }
    
    /**
     * A more strongly-typed version of request().
     * 
     * This works pretty much exactly like request, but expects that the response will be of type T. It will
     * throw a ClassCastException if anything else is received. Otherwise, it is identical to request().
     */
    def requestFor[T](msg:Any, retriesInit:Int = 0)(implicit tag: ClassTag[T], enclosing:sourcecode.FullName, file:sourcecode.File, line:sourcecode.Line):RequestM[T] = {
      val req = new RequestM[T](enclosing, "requestFor", file, line)
      requester.doRequest[T](target, msg, req)
      var retries = retriesInit
      req.intercept {
        // We replace AskTimeoutException with retries, and maybe with RequestRetriesExhausted.
        // TODO: there is probably a better way to do this, but it likely involves inventing something
        // like RequestM.transform().
        case Failure(ex:AskTimeoutException) => {
          if (retries > 0) {
            retries -= 1
            requester.doRequest[T](target, msg, req)
            true
          } else {
            // No, really -- we're giving up and letting the timeout happen:
            false
          }
        }
        case _ => false
      }
      req
    }
    
    /**
     * ask-style syntax for ordinary requests.
     * 
     * This *intentionally* conflicts with akka.pattern.ask, on the grounds that if you're in an askable situation,
     * it is generally a bug to be using raw asks. So if you wind up with ambiguity, that's a warning sign.
     * 
     * (I might be willing to break this out, to make it possible to work around it, but somebody's going to have
     * to convince me it's a good idea to do so. If you really want to use ask, then spell it out explicitly.)
     */
    def ?(msg:Any):RequestM[Any] = request(msg)
  }
  
  /**
   * Similar to RequestableActorRef, but works with an ActorSelection.
   */
  implicit class RequestableActorSelection(target:ActorSelection) {
    def request(msg:Any):RequestM[Any] = {
      requestFor[Any](msg)
    }
    
    def requestFor[T](msg:Any)(implicit tag: ClassTag[T], enclosing:sourcecode.FullName, file:sourcecode.File, line:sourcecode.Line):RequestM[T] = {
      val req = new RequestM[T](enclosing, "requestFor", file, line)
      requester.doRequestGuts[T](target.ask(msg)(requester.requestTimeout), req)
      req
    }
    
    def ?(msg:Any):RequestM[Any] = request(msg)
  }
  
  /**
   * Convert a Future into a Request.
   * 
   * This takes the specified Future, and runs it in the Requester's main loop, to make it properly safe. As usual,
   * sender will be preserved.
   * 
   * This is implicit, so if you are in a context that already expects a Request (such as a for comprehension with a Request
   * at the top), it will quietly turn the Future into a Request. If Request isn't already expected, though, you'll have
   * to specify loopback explicitly.
   */
  implicit def loopback[T](f:Future[T])(implicit tag:ClassTag[T], enclosing:sourcecode.FullName, file:sourcecode.File, line:sourcecode.Line):RequestM[T] = {
    val req = new RequestM[T](enclosing, "loopback", file, line)
    requester.doRequestGuts[T](f, req)
    req
  }
  
  /**
   * Convert a Request into a Future.
   * 
   * Sometimes, at the edges of the API, you need to think in terms of Futures. When this is necessary,
   * this implicit will take your RequestM and turn it into a Future of the matching type.
   */
  implicit def request2Future[T](req:RequestM[T]):Future[T] = {
    val promise = Promise[T]
    req onComplete {
      case Success(v) => promise.success(v)
      case Failure(ex) => promise.failure(ex)
    }
    promise.future
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
 * Note that, to make this work, the Request trait mixes in its own version of unhandled(). For this to
 * work properly, therefore, it is very important that, if your own Actor overrides unhandled, it calls
 * super.unhandled() for unknown messages!
 * 
 * That unhandled() override is usually enough to catch the looped-back messages, so you usually just
 * need to mix Requester into your Actor. However, if your Actor's receive function is intercepting *all*
 * messages (so nothing makes it to unhandled), then you will need to call handleRequestResponse at the
 * beginning of your receive; otherwise, your Actor can wind up deadlocked. This can particularly happen
 * when using stash() during Actor startup:
 * {{{
 * def receive = handleRequestResponse orElse {
 *   case Start => {
 *     persistence.request(LoadMe(myId)) foreach { myState =>
 *       setState(myState)
 *       unstashAll()
 *       become(mainReceive)
 *     }
 *   }
 *   
 *   case _ => stash()
 * }
 * }}}
 * In this startup pattern, we are stashing all messages until the persister responds with the Actor's state.
 * However, if we didn't have handleRequestResponse there, the response would also get stashed, so the
 * Actor would never receive the state message, and the Actor would be stuck.
 * 
 * IMPORTANT: Requester is *not* compatible with stateful versions of become() -- that is, if you are
 * using become() in a method where you are capturing the parameters in the closure of become(),
 * Requester will probably not work right. This is because the body of the response handler will capture
 * the closed-over parameter, and if the Actor has become() something else in the meantime, the handler
 * will use the *old* data, not the new.
 * 
 * More generally, Requester should be used with caution if your Actor changes state frequently.
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
   * 
   * TODO: this is suspicious, since it does not follow Akka's preferred pattern for timeouts.
   * We might change how this works.
   */
  implicit val requestTimeout = Timeout(10.seconds)
 
  /**
   * Send a request, and specify the handler for the received response. You may also specify a failHandler,
   * which will be run if the operation fails for some reason. (Most often, because we didn't receive a
   * response within the timeout window.)
   */
  def doRequest[T](otherActor:ActorRef, msg:Any, handler:RequestM[T])(implicit tag: ClassTag[T]) = {
    doRequestGuts(otherActor ask msg, handler)
  }
  
  def doRequestGuts[T](f:Future[Any], handler:RequestM[T])(implicit tag: ClassTag[T]) = {
    val originalSender = sender
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
  
  /**
   * Normally you don't need to invoke this manually -- Requester defines an unhandled()
   * override that deals with these responses. But if your receive method intercepts *all*
   * messages for some reason (for example, it stashes everything), then you should add
   * this at the front of your receive so that it deals with responses.
   */
  def handleRequestResponse:Receive = {
    case resp:RequestedResponse[_] => resp.invoke
  }
  
  abstract override def unhandled(message: Any): Unit = {
    message match {
      case resp:RequestedResponse[_] => resp.invoke
      case other => super.unhandled(other)
    }
  }
}
