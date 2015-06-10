package querki.photos

import models.{OID, Wikitext}

import querki.values.RequestContext

object PhotoUploadMessages {
  case class PhotoChunk(chunk:Array[Byte])
  
  case class UploadDone(rc:RequestContext, propId:OID, thingId:OID)
  
  case class PhotoInfo(wikitext:Wikitext)
  case object PhotoFailed
}