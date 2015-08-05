package querki.streaming

import scala.concurrent.duration.FiniteDuration

import querki.values.RequestContext

object UploadMessages {
  case class UploadChunk(chunk:Array[Byte])
  case class UploadComplete(rc:RequestContext)
  
  case object GetUploadTimeout
  case class UploadTimeout(timeout:FiniteDuration)
  
  case class UploadProcessSuccessful(response:String)
  case class UploadProcessFailed(ex:String)
}
