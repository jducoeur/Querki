package querki.streaming

import scala.concurrent.duration.FiniteDuration

import akka.util.ByteString

import querki.values.RequestContext

object UploadMessages {
  /**
   * NOTE: this is coming from a ByteString, and it seems like we might as well just send a ByteString.
   * We're converting and sending Vector[Byte] instead because I've found empirically that something is
   * *deeply* broken in serialization of ByteString, such that the receiving end is frequently getting
   * a corrupted, null-filled version.
   */
  case class UploadChunk(index:Int, chunk:Vector[Byte])
  case class UploadChunkAck(index:Int, currentTotalSize:Int)
  case class UploadComplete(rc:RequestContext)
  
  case object GetUploadTimeout
  case class UploadTimeout(timeout:FiniteDuration)
  
  case class UploadProcessSuccessful(response:String)
  case class UploadProcessFailed(ex:String)
}
