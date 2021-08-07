package querki.streaming

import scala.concurrent.duration._

import akka.actor._
import akka.event.LoggingReceive

import querki.globals._
import querki.util.QuerkiActor

/**
 * This pair of classes represents a simple streaming protocol for sending potentially large Strings
 * between cooperating Actors that may be on different nodes. It is liberally adapted from
 * StreamSendActor and UploadActor, and we should probably see about lifting some abstractions
 * from all of this, but they are just different enough to not make that trivial.
 *
 * Note that this is *not* a particularly smart or efficient protocol -- it's reasonably reliable,
 * but since it's purely pull-based it's going to be high-latency. I am actively hoping that
 * Lightbend adds a decent back-pressured stream abstraction into the standard Akka library,
 * thus obviating this whole mess. If not, we may want to put a properly back-pressured protocol
 * into place here, to speed things up.
 */
object StringStream {

  def receiverProps(sender: ActorRef) = Props(classOf[StringStreamReceiver], sender)
  def senderProps(str: String)(implicit e: Ecology) = Props(classOf[StringStreamSender], str, e)

  case object Start
  case class ReceivedString(str: String)

  case object Ready

  case class StringChunk(
    str: String,
    index: Int
  )
  case class Ack(index: Int)

  case class ChunkTimeout(
    index: Int,
    attempt: Int
  )

  case class Complete(index: Int)
  case object SendComplete
}
import StringStream._

private[streaming] class StringStreamSender(
  str: String,
  e: Ecology
) extends Actor
     with EcologyMember {

  implicit val ecology = e

  val chunkSize = Config.getInt("querki.stream.stringChunkSize", 10000)

  var remainingChunks = str.grouped(chunkSize).toSeq.zipWithIndex

  var currentTimeout: Option[Cancellable] = None

  var currentIndex: Int = -1

  var originator: Option[ActorRef] = None

  var _recipient: Option[ActorRef] = None
  def recipient = _recipient.get

  def sendCurrentChunk(attempt: Int) = {
    val (chunk, index) = remainingChunks.head
    currentIndex = index
    recipient ! StringChunk(chunk, index)
    currentTimeout = Some(context.system.scheduler.scheduleOnce(2 seconds, self, ChunkTimeout(currentIndex, attempt)))
  }

  def sendComplete(attempt: Int) = {
    recipient ! Complete(currentIndex)
    currentTimeout = Some(context.system.scheduler.scheduleOnce(2 seconds, self, ChunkTimeout(currentIndex, attempt)))
  }

  def receive = LoggingReceive {

    /**
     * This just exists to tell us who to send the ack to when we are finished:
     */
    case Start => {
      originator = Some(sender)
    }

    /**
     * This comes from the StringStreamReceiver at the other end. Time to start sending:
     */
    case Ready => {
      _recipient = Some(sender)
      sendCurrentChunk(0)
    }

    /**
     * The receiver has gotten a block...
     */
    case Ack(index) => {
      if (index == currentIndex) {
        // Okay, it's the current block...
        currentTimeout.map(_.cancel())
        currentTimeout = None
        if (remainingChunks.isEmpty) {
          // That was the final Ack, for the Complete, so we're done. Tell the originator that we're
          // finished, and die:
          // NOTE: there's a potential (if rare) race condition here, which I've seen happen once.
          // It is only likely if sender and receiver are on the same machine, but if so, we *can*
          // get all the way through the sending process and to here before receiving our original
          // Start message. In that case, this won't happen, and the originator won't ever receive
          // the completion. So the originator should always have a timeout for this.
          originator.map(_ ! SendComplete)
          context.stop(self)
        } else {
          remainingChunks = remainingChunks.drop(1)
          if (remainingChunks.isEmpty) {
            // That was the ack of the last block, so send out the Complete:
            currentIndex += 1
            sendComplete(0)
          } else {
            // More to go:
            sendCurrentChunk(0)
          }
        }
      } else {
        // It's an obsolete Ack, so ignore it. This can happen when things get delayed and resent.
      }
    }

    // We've sent a block, but haven't gotten an Ack in sufficient time...
    case ChunkTimeout(index, attempt) => {
      if (index == currentIndex) {
        if (attempt >= 3) {
          if (remainingChunks.isEmpty) {
            // This can happen if the Ack from the Receiver didn't get through, and now it's dead.
            // We will optimistically assume that everything's okay.
            originator.get ! SendComplete
          } else {
            // Something's seriously wrong; give up
            QLog.error("StringStreamSender failed to send string, with ${remainingChunks.size} remaining: $str")
          }
          // Either way, we're out of work to do:
          context.stop(self)
        } else {
          // Try, try again:
          if (remainingChunks.isEmpty)
            sendComplete(attempt + 1)
          else
            sendCurrentChunk(attempt + 1)
        }
      } else {
        // Obsolete timeout. We don't care, although that's a bit weird. Is this race condition
        // actually possible?
      }
    }
  }
}

private[streaming] class StringStreamReceiver(stringSender: ActorRef) extends Actor {

  /**
   * This is the Actor (typically an ask pseudo-Actor) to which we will be returning the string
   * once it is reconstructed.
   */
  var receiver: Option[ActorRef] = None

  var builder = StringBuilder.newBuilder

  var latestIndex: Int = -1

  def receive = {
    // This is the "boot" message, which tells us who to report the eventual results to.
    case Start => {
      receiver = Some(sender)
      // Okay, tell the Sender to start sending us blocks:
      stringSender ! Ready
    }

    // This is a block of text from the Sender:
    case StringChunk(chunk, index) => {
      if (index > latestIndex) {
        // It's the next one, so tack it on:
        builder.append(chunk)
        latestIndex = index
      } else {
        // Looks like a resend of a chunk we've already processed. Ack it, but otherwise don't worry
      }
      sender ! Ack(index)
    }

    // Sender says we are done:
    case Complete(index) => {
      sender ! Ack(index)
      // Note that we assume that this message is local, and thus more or less reliable:
      receiver.map(_ ! ReceivedString(builder.toString))
      // And we die. Note that there is a chance that the Sender won't get the Ack; Sender is built
      // to cope with this edge case.
      context.stop(self)
    }
  }
}
