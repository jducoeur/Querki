package querki.streaming

import querki.values.RequestContext

object UploadMessages {
  case class UploadChunk(chunk:Array[Byte])
  case class UploadComplete(rc:RequestContext)
}
