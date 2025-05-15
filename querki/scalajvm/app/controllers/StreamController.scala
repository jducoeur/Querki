package controllers

import akka.actor._
import akka.util.ByteString
import play.api.mvc._
import querki.globals._
import querki.streaming._
import UploadMessages._
import akka.stream.Materializer
import akka.stream.scaladsl.Sink
import play.api.libs.streams.Accumulator

/**
 * This controller mixin adds the core machinery for streaming through a controller to a receiving Actor.
 *
 * An underlying assumption here is that we start with the *parent* of the receiving Actor, and construct an
 * Actor for this upload operation. This is generally going to be our recommended practice, since streaming
 * is a very heavy operation.
 *
 * @author jducoeur
 */
trait StreamController { self: ApplicationBase =>

  /**
   * This is the most subtle and important plumbing in the process of uploading. This is used in BodyParser,
   * which means that it produces an Accumulator that accept ByteStrings (chunks of a file), and returns a "body"
   * that is actually an ActorRef.
   *
   * This expects the startupFunc to produce an ActorRef for an UploadActor. That implements the receiving end
   * of Akka Streams' Sink.actorRefWithAck(), which we use to generate the sending side.
   */
  def uploadBodyChunks(
    startupFunc: => Future[ActorRef]
  )(
    rh: RequestHeader
  )(implicit
    mat: Materializer
  ): Accumulator[ByteString, Either[Result, ActorRef]] = {
    // The actual destination Actor, which may be anywhere in the cluster:
    val workerRefFuture: Future[ActorRef] = startupFunc

    // This is largely just wrapper code to go from the built-in Sink constructor to a nice normal
    // Accumulator:
    val futureAccumulator: Future[Accumulator[ByteString, Either[Result, ActorRef]]] =
      workerRefFuture.map { workerRef =>
        val sink: Sink[ByteString, Future[Either[Result, ActorRef]]] = Sink.actorRefWithAck(
          workerRef,
          StreamInitialized,
          AckMessage,
          StreamComplete,
          (ex: Throwable) => OnFailure(ex)
        ).mapMaterializedValue(_ => Future(Right(workerRef)))

        Accumulator(sink)
      }

    Accumulator.flatten(futureAccumulator)
  }
}
