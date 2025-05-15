package querki.streaming

import scala.concurrent.duration.FiniteDuration

import akka.util.ByteString

import querki.values.RequestContext

object UploadMessages {

  // Standard message types required by Sink.actorRefWithAck -- see
  //   https://doc.akka.io/libraries/akka-core/2.5/stream/stream-integrations.html#sink-actorrefwithack

  // Sent to say that we're ready to start sending
  case object StreamInitialized

  // Response from the UploadActor to the sender; sent for each message
  case object AckMessage

  // Sent when the stream finishes
  case object StreamComplete

  // Sent when the stream hits an error
  case class OnFailure(ex: Throwable)

  // Our extension to the protocol: when everything is uploaded, add the metadata and kick things off. This is
  // called at the semantic layer (usually the controller), since the metadata type depends on what we're
  // trying to do here.
  case class ProcessUpload[T](t: T)
}
