package controllers

import scala.concurrent.duration._

import akka.actor._
import akka.pattern.ask
import akka.util.{ByteString, Timeout}

import play.api.libs.iteratee._
import play.api.mvc._

import querki.globals._
import querki.streaming._
import UploadMessages._

/**
 * This controller mixin adds the core machinery for streaming through a controller to a receiving Actor.
 * 
 * An underlying assumption here is that we start with the *parent* of the receiving Actor, and construct an
 * Actor for this upload operation. This is generally going to be our recommended practice, since streaming
 * is a very heavy operation.
 * 
 * @author jducoeur
 */
trait StreamController { self:ApplicationBase =>

  /**
   * This is the most subtle and important plumbing in the process of uploading. This is a BodyParser,
   * which means that it produces an Iteratee that accept Byte Arrays (chunks of a file), and returns a "body"
   * that is actually a Future[ActorRef].
   * 
   * What happens in here is that it calls the given start function, which should return a pointer to an
   * UploadActor. We then create a fancy Iteratee that
   * keeps folding over the input chunks, producing a new Future each time. These Futures always contains the
   * same thing -- the ActorRef -- but we don't get to the final one until we've processed all of the bytes.
   * 
   * TODO: all of this should now be rewritten in terms of Akka Streams -- ideally, we should be directly
   * streaming to the target Actor, although I'm not sure that's yet possible. But getting the reliability and
   * back-pressure would be a Big Win.
   */
  def uploadBodyChunks(startupFunc: => Future[ActorRef])(rh:RequestHeader):Iteratee[ByteString, Either[Result, Future[ActorRef]]] = {
    // The actual destination Actor, which may be anywhere in the cluster:
    val workerRefFuture:Future[ActorRef] = startupFunc
    // A local worker, which implements this side of a reliable streaming protocol to that destination:
    val senderRefFuture = workerRefFuture.map { workerRef =>
      SystemManagement.actorSystem.actorOf(StreamSendActor.actorProps(workerRef))
    }
    
    implicit val t = Timeout(10 seconds)
      
    // Now, fold over all of the chunks, producing a new Future each time
    // TODO: we ought to put a limit on the total number of bytes we are willing to send here, to avoid
    // DOS attacks
    // TODO: this is currently implementing the most braindead reliable streaming protocol, by waiting for
    // acks for each chunk. This is a *bad* protocol -- it's fundamentally slow from a latency POV -- but
    // it's easy and reliable, so we'll do it for now. It should be replaced by a more real streaming protocol
    // when we get a chance, preferably something official once the Akka Team deals with it.
    Iteratee.fold[ByteString, Future[ActorRef]](senderRefFuture) { (fut, bytes) => 
      val newFut = fut.flatMap { ref => 
        // Note that this is sending to the local StreamSendActor, which deals with the
        // exactly-once delivery semantics:
        ref.ask(bytes).map { case UploadChunkAck(index, total) => ref }
      }
      newFut
    } map { fut =>
      // Finish up the sender, and then throw it away, replacing it with the actual worker:
      fut.map { senderRef => senderRef ! StreamSendActor.StreamCompleted }
      Right(fut.flatMap(_ => workerRefFuture))
    }
  }
}
