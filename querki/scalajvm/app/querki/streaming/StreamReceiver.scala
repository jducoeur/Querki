package querki.streaming

import akka.actor._

import querki.globals._

/**
 * Actor mix-in to retrieve a Stream of serializable objects from a StreamSender.
 * 
 * This can currently only cope with one stream at a time. We'd need to enhance the
 * protocol to make it cope with multiple incoming streams.
 * 
 * Note that this works by intercepting unhandled(), so it does not play nicely with
 * stash() at the moment.
 * TODO: post-Akka-2.4, rewrite this in terms of the Receive Pipeline.
 * 
 * @author jducoeur
 */
trait StreamReceiver extends Actor {
  abstract override def unhandled(message: Any): Unit = {
    message match {
//      case resp:RequestedResponse[_] => resp.invoke
      case other => super.unhandled(other)
    }
  }  
}
