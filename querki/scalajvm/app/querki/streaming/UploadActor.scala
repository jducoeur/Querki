package querki.streaming

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
  
  def handleChunks:Receive = {
    case UploadChunk(chunk) => {
      chunkBuffer = chunkBuffer ++ chunk
      QLog.spew(s"Actor got ${chunk.length} bytes")
    }    
  }
}