package querki.photos

import models.Property

import querki.values.{QValue, SpaceState}

object PhotoUploadMessages {
  case class PhotoChunk(chunk:Array[Byte])
  
  case class UploadDone(oldValue:Option[QValue], prop:Property[_,_], state:SpaceState)
  
  case class PhotoInfo(newValue:QValue)
}