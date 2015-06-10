package querki.photos

import models.Property

import querki.values.{QValue, SpaceState}

object PhotoUploadMessages {
  case class PhotoChunk(chunk:Array[Byte])
  
  case class UploadDone(oldValue:Option[QValue], prop:Property[_,_])
  
  case class PhotoInfo(newValue:QValue)
}