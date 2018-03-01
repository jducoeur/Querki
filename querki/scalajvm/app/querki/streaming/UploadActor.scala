package querki.streaming

import java.io.{ByteArrayInputStream}
import java.util.zip.GZIPInputStream

import scala.concurrent.duration._
import scala.io.Source

import akka.actor._

import UploadMessages._

import querki.globals._
import querki.values.RequestContext

/**
 * This mixin trait receives chunks from a StreamController. It should get mixed in with an
 * Actor that implements processBuffer(). It builds up the chunkBuffer, which the real
 * code can then make use of.
 * 
 * This expects that it is receiving messages from a StreamSendActor; the two work together
 * to implement a simplistic but reliable byte-oriented streaming protocol.
 * 
 * @author jducoeur
 */
trait UploadActor { self:Actor =>
  /**
   * The raw buffer, which is mutated as the stream uploads.
   */
  var chunkBuffer:Vector[Byte] = Vector.empty
  
  /**
   * The index of the chunk we most recently received. This is for deduping retries.
   */
  var lastIndex:Int = -1
  
  var uploadComplete:Boolean = false

  // The 16-bit header, which may indicate zippiness:
  private lazy val magicHead = {
    if (chunkBuffer.length < 2)
      0
    else
      (chunkBuffer(0).toInt & 0xff | (chunkBuffer(1).toInt << 8) & 0xff00)
  }
  /**
   * Returns true iff the chunkBuffer appears to be gzip'ped.
   */
  private lazy val isGZip = (GZIPInputStream.GZIP_MAGIC == magicHead)
  
  private lazy val baseStream = new ByteArrayInputStream(chunkBuffer.toArray)
  
  /**
   * The uploaded data, as a stream.
   * 
   * Iff the uploaded data was GZip'ped, that will be handled under the hood.
   * 
   * For the time being, this should only be called once uploading is complete -- we don't
   * support progressive loading yet.
   */
  lazy val uploadedStream = {
    if (isGZip) {
      new GZIPInputStream(baseStream)
    } else {
      baseStream
    }
  }
  
  /**
   * The uploaded data, as a String. Unzipping will be handled quietly before we get to this point.
   * 
   * This is typically the easiest way to handle the input.
   */
  lazy val uploaded = Source.fromInputStream(uploadedStream).mkString
  
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
    case UploadChunk(index, chunk) => {
      if (index > lastIndex) {
        chunkBuffer = chunkBuffer ++ chunk
//        QLog.spew(s"Actor got block $index, ${chunk.length} bytes: length ${chunk.size} ${chunk.take(4)} ... ${chunk.takeRight(4)}")
        lastIndex = index
        sender ! UploadChunkAck(index, chunkBuffer.size)
      } else {
//        QLog.spew(s"Received duplicate of chunk #$index")
        // Although it's a duplicate, we still need to re-ack, in case our previous ack got
        // lost in transit:
        sender ! UploadChunkAck(index, chunkBuffer.size)
      }
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
