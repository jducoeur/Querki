package models

import Thing._

// TODO: is this enumeration worthwhile? Maybe I should just use MIME type instead.
object AttachmentKind {
  type AttachmentKind = Int
  
  val CSS = 0
  val JPEG = 1
}

object Attachments {

}
