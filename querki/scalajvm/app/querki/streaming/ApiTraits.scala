package querki.streaming

import scala.concurrent.duration.FiniteDuration

import akka.util.ByteString

import querki.values.RequestContext

object UploadMessages {
  case class UploadChunk(index:Int, chunk:ByteString)
  case class UploadChunkAck(index:Int, currentTotalSize:Int)
  case class UploadComplete(rc:RequestContext)
  
  case object GetUploadTimeout
  case class UploadTimeout(timeout:FiniteDuration)
  
  case class UploadProcessSuccessful(response:String)
  case class UploadProcessFailed(ex:String)
}
