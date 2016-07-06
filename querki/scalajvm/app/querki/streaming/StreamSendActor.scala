package querki.streaming

import scala.concurrent.duration._

import akka.actor._
import akka.util.{ByteString, Timeout}

import querki.globals._

import UploadMessages._

/**
 * This is a simplistic Actor, used to implement the sending side of our reliable streaming protocol. Neither
 * ask nor request are sufficient to the task, because managing possible duplication gets hairy in an
 * exactly-once protocol such as we need here.
 * 
 * Note an important assumption: since the StreamSendActor is created *locally* to the Controller, so
 * asks from the Controller to this Actor are adequately reliable.
 * 
 * The caller of this sender should be creating it locally, and using ask() to send it ByteStrings. It must
 * wait until it receives an UploadChunkAck before sending the next, and should have a timeout of at least four
 * times the timeout of the retry in here.
 * 
 * TODO: this needs unit testing!
 * 
 * @param recipient An UploadActor that will be receiving the stream.
 */
class StreamSendActor(recipient:ActorRef) extends Actor {
  
  import StreamSendActor._
  
  var currentIndex = 1
  
  var currentBlock:Option[ByteString] = None
  
  /**
   * This is the ask mini-Actor inside uploadBodyChunks that we are currently responding to.
   */
  var currentAsk:Option[ActorRef] = None
  
  var currentTimeout:Option[Cancellable] = None
  
  def sendBlock(attempt:Int) = { 
    QLog.spew(s"Sending block #$currentIndex, attempt #$attempt: length ${currentBlock.get.size} ${currentBlock.get.take(4)} ... ${currentBlock.get.takeRight(4)}")
    recipient ! UploadChunk(currentIndex, currentBlock.get)
    currentTimeout = Some(context.system.scheduler.scheduleOnce(2 seconds, self, StreamChunkTimeout(currentIndex, attempt)))
  }
  
  def receive = {
    case bytes:ByteString => {
      currentAsk = Some(sender)
      currentBlock = Some(bytes)
      sendBlock(0)
    }
    
    case ack @ UploadChunkAck(index, totalSize) => {
      if (index == currentIndex) {
        // Okay, the current chunk is acknowledged, so send that back to the asker:
        currentAsk.map(_ ! ack)
        currentAsk = None
        currentIndex += 1
        currentTimeout.map(_.cancel())
        currentTimeout = None
        currentBlock = None
      } else {
        // It's an obsolete Ack -- in theory, we don't care
      }
    }
    
    case StreamChunkTimeout(index, attempt) => {
      if (index == currentIndex) {
        // Current attempt has timed out. How many times have we tried?
        if (attempt >= 3) {
          // Looks like something is very wrong here
          QLog.error(s"StreamSendActor has failed to send a chunk to the recipient! Attempt #$attempt at block #$index")
          // TODO: we should do something more pro-active to push the failure up the line; for now, we're
          // just letting the original ask time out
        } else {
          // Okay, let's try again:
          sendBlock(attempt + 1)
        }
      } else {
        // It's an obsolete timeout -- in theory, we don't care
      }
    }
    
    case StreamCompleted => {
      context.stop(self)
    }
  }
}

object StreamSendActor {
  
  def actorProps(dest:ActorRef) = Props(classOf[StreamSendActor], dest)
  
  /**
   * Signifies that our attempt to send this chunk has timed out.
   */
  case class StreamChunkTimeout(index:Int, attemptNumber:Int)
  case object StreamCompleted
}
