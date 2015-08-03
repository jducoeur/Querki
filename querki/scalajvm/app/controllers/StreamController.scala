package controllers

import scala.concurrent.Future
import scala.concurrent.duration._

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout

import play.api.libs.iteratee._
import play.api.mvc._

import querki.globals._
import Implicits._
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
   * What happens in here is that it sends the start message to the specified parent. That causes an
   * UploadActor to be created, and we get back an ActorRef to it. We then create a fancy Iteratee that
   * keeps folding over the input chunks, producing a new Future each time. These Futures always contains the
   * same thing -- the ActorRef -- but we don't get to the final one until we've processed all of the bytes.
   */
  def uploadBodyChunks(startupFunc: => Future[(ActorRef, Any)])(rh:RequestHeader):Iteratee[Array[Byte], Either[Result, Future[ActorRef]]] = {
    implicit val timeout = Timeout(5 seconds)

    val workerRefFuture:Future[ActorRef] = for {
      (uploadParent, startMsg) <- startupFunc
      workerRef <- (uploadParent ? startMsg).mapTo[ActorRef]
    }
      yield workerRef
      
    // Now, fold over all of the chunks, producing a new Future each time
    // TODO: we ought to put a limit on the total number of bytes we are willing to send here, to avoid
    // DOS attacks:
    Iteratee.fold[Array[Byte], Future[ActorRef]](workerRefFuture) { (fut, bytes) => 
      val newFut = fut.map { ref => ref ! UploadChunk(bytes); ref }
      newFut
    } map (fut => Right(fut))
  }
}