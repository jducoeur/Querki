package querki.streaming

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.util.zip.GZIPInputStream
import scala.concurrent.duration._
import scala.io.Source
import akka.actor._
import UploadMessages._
import querki.globals._
import querki.values.RequestContext

import scala.collection.mutable.ArrayBuffer

/**
 * This mixin trait receives chunks from a StreamController. It should get mixed in with an
 * Actor that implements processBuffer(). It builds up the chunkBuffer, which the real
 * code can then make use of.
 *
 * This expects that it is receiving messages from a StreamSendActor; the two work together
 * to implement a simplistic but reliable byte-oriented streaming protocol.
 *
 * All of this is deeply horrible -- it and its subclasses should be properly stream-oriented,
 * instead of doing all this in-memory buffering.
 *
 * @author jducoeur
 */
trait UploadActor { self: Actor =>

  /**
   * The index of the chunk we most recently received. This is for deduping retries.
   */
  var lastIndex: Int = -1

  /**
   * How many bytes have we received so far?
   */
  var bytesSoFar: Int = 0

  var uploadComplete: Boolean = false

  /**
   * Returns true iff the stream appears to be gzip'ped.
   *
   * We determine this based on the first 16 bits of the stream.
   */
  private def isGZip(stream: ByteArrayInputStream): Boolean = {
    // Peek at the first two bytes:
    val twoBytes = new Array[Byte](2)
    if (stream.read(twoBytes, 0, 2) == -1) {
      // We didn't even get two bytes, so it's not GZip
      false
    } else {
      stream.reset()
      val magicHead = (twoBytes(0).toInt & 0xff | (twoBytes(1).toInt << 8) & 0xff00)
      GZIPInputStream.GZIP_MAGIC == magicHead
    }
  }

  /**
   * We write our chunks into here as they arrive.
   */
  private val outputStream = new ByteArrayOutputStream()

  /**
   * The uploaded data, as a stream.
   *
   * Iff the uploaded data was GZip'ped, that will be handled under the hood.
   *
   * For the time being, this should only be called once uploading is complete -- we don't
   * support progressive loading yet.
   */
  lazy val uploadedStream = {
    val baseStream = new ByteArrayInputStream(outputStream.toByteArray)
    if (isGZip(baseStream)) {
      QLog.spew("Received GZip stream")
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
  lazy val uploaded = {
    val str = Source.fromInputStream(uploadedStream).mkString
    QLog.spew(s"Size of uploaded String: ${str.length}")
    str
  }

  /**
   * This will be called once the upload is finished. The UploadActor must define this function,
   * which should take the chunkBuffer and do something with it.
   *
   * processBuffer() should typically return its values to sender using UploadProcessSuccessful
   * and UploadProcessFailed, and then call context.stop(self), but this is left to the
   * individual case to decide.
   */
  def processBuffer(rc: RequestContext): Unit

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

  def handleChunks: Receive = {
    case UploadChunk(index, chunk) => {
      if (index > lastIndex) {
        outputStream.write(chunk.toArray)
        bytesSoFar += chunk.size
//        QLog.spew(
//          s"Actor got block $index, ${chunk.length} bytes: length ${chunk.size} ${spewChunks(chunk)}"
//        )
        lastIndex = index
        sender ! UploadChunkAck(index, bytesSoFar)
      } else {
        QLog.spew(s"UploadActor: Received duplicate of chunk #$index")
        // Although it's a duplicate, we still need to re-ack, in case our previous ack got
        // lost in transit:
        sender ! UploadChunkAck(index, bytesSoFar)
      }
    }

    case GetUploadTimeout => {
      sender ! UploadTimeout(processTimeout)
    }

    case UploadComplete(rc) => {
      uploadComplete = true

      QLog.spew(s"Upload complete!")
      QLog.spew(s"Bytes received: $bytesSoFar")
      QLog.spew(s"Allocated size of the outputStream: ${outputStream.size()}")

      processBuffer(rc)
    }
  }

  /**
   * Default implementation of receive. The UploadActor may override this, but must call
   * handleChunks in order to function properly.
   */
  def receive = handleChunks
}
