package querki.streaming

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.util.zip.GZIPInputStream
import scala.io.Source
import akka.actor._
import UploadMessages._
import akka.util.ByteString
import querki.globals._

/**
 * This mixin trait receives chunks from a StreamController. It should get mixed in with an
 * Actor that implements processBuffer(). It builds up the chunkBuffer, which the real
 * code can then make use of.
 *
 * This expects that it is receiving messages from Akka Streams -- it is the receiving end of
 * Sink.actorRefWithAck(), with a bit of enhancement to send the metadata that we usually need.
 *
 * @author jducoeur
 * @tparam MetadataType the type of the metadata for this UploadActor, received at the end
 */
trait UploadActor[MetadataType] { self: Actor =>

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
  def processBuffer(metadata: MetadataType): Unit

  // Implements the receiving end of Sink.actorRefWithSink(). See
  //   https://doc.akka.io/libraries/akka-core/2.5/stream/stream-integrations.html#sink-actorrefwithack
  def handleChunks: Receive = {
    // We don't need to do anything special to start things up:
    case StreamInitialized => sender ! AckMessage

    case bytes: ByteString => {
      outputStream.write(bytes.toArray)
      bytesSoFar += bytes.size
      sender ! AckMessage
    }

    case StreamComplete => {
      uploadComplete = true

      QLog.spew(s"Upload complete!")
      QLog.spew(s"Bytes received: $bytesSoFar")
      QLog.spew(s"Allocated size of the outputStream: ${outputStream.size()}")
    }

    case OnFailure(ex) => {
      QLog.error("Upload failed!", ex)
      // Is there anything else useful to do here?
    }

    // Our extension to the protocol: the semantic layer should kick this off at the end, adding any metadata
    // needed in order to process this uploaded file. This is when the actual work happens.
    case ProcessUpload(metadataRaw) => {
      QLog.spew(s"Beginning processing...")
      // Ugly, but needed to put the types together, and avoids erasure warnings. Will error at runtime if we
      // get it wrong. (This is one of those places where we could probably do better with Akka Typed.)
      val metadata = metadataRaw.asInstanceOf[MetadataType]
      processBuffer(metadata)
    }
  }

  /**
   * Default implementation of receive. The UploadActor may override this, but must call
   * handleChunks in order to function properly.
   */
  def receive = handleChunks
}
