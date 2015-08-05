package querki.streaming

import scala.concurrent.duration._

import akka.actor._

import UploadMessages._

import querki.globals._

/**
 * This mixin trait receives chunks from a StreamController. It should get mixed in with an
 * Actor that uses handleChunks in its receive. It builds up the chunkBuffer, which the real
 * code can then make use of.
 * 
 * @author jducoeur
 */
trait UploadActor { self:Actor =>
  var chunkBuffer:Vector[Byte] = Vector.empty
  
  /**
   * How long to allow this Actor to *process* the uploaded data. Subclasses may override this
   * as needed. Note that this value will be fetched and used by the upload() entry point in
   * ClientController.
   */
  val processTimeout = 1 minute
  
  def handleChunks:Receive = {
    case GetUploadTimeout => {
      sender ! UploadTimeout(processTimeout)
    }
    
    case UploadChunk(chunk) => {
      chunkBuffer = chunkBuffer ++ chunk
      QLog.spew(s"Actor got ${chunk.length} bytes")
    }    
  }
}