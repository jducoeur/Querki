package querki.streaming

import scala.concurrent.duration._

import akka.actor._

import UploadMessages._

import querki.globals._
import querki.values.RequestContext

/**
 * This mixin trait receives chunks from a StreamController. It should get mixed in with an
 * Actor that implements processBuffer(). It builds up the chunkBuffer, which the real
 * code can then make use of.
 * 
 * @author jducoeur
 */
trait UploadActor { self:Actor =>
  var chunkBuffer:Vector[Byte] = Vector.empty
  
  var uploadComplete:Boolean = false
  
  /**
   * This will be called once the upload is finished. The UploadActor must define this function,
   * which should take the chunkBuffer and do something with it.
   * 
   * processBuffer() should typically return its values to sender using UploadProcessSuccessful
   * and UploadProcessFailed, and then call context.stop(self), but this is left to the
   * individual case to decide.
   */
  def processBuffer(rc:RequestContext):Unit
  
  /**
   * How long to allow this Actor to *process* the uploaded data. Subclasses may override this
   * as needed. Note that this value will be fetched and used by the upload() entry point in
   * ClientController.
   * 
   * Note that this is intentionally a def rather than a val, so that sophisticated UploadActors
   * can do things like set the timeout proportional to the size of the uploaded file. This
   * timeout will be fetched after all chunks have been loaded, but before processing begins.
   */
  def processTimeout = 1 minute
  
  def handleChunks:Receive = {
    case UploadChunk(chunk) => {
      chunkBuffer = chunkBuffer ++ chunk
      QLog.spew(s"Actor got ${chunk.length} bytes")
    }
    
    case GetUploadTimeout => {
      sender ! UploadTimeout(processTimeout)
    }
    
    case UploadComplete(rc) => {
      uploadComplete = true
      processBuffer(rc)
    }
  }
  
  /**
   * Default implementation of receive. The UploadActor may override this, but must call
   * handleChunks in order to function properly.
   */
  def receive = handleChunks
}
