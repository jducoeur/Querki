package querki.test.mid

import querki.data.{ThingInfo, TID}

/**
 * Describes a value that can be "saved" through the Client API.
 */
trait SaveableVal {
  def toSave: Seq[String]
}

case class SaveableText(str: String) extends SaveableVal {
  def toSave = List(str)
}

/**
 * Typeclass for creating a SaveablePropVal from another type.
 */
trait Saveable[T] {
  def toSaveable(t: T): SaveableVal
}

object Saveable {
  implicit object StringSaveable extends Saveable[String] {
    def toSaveable(t: String) = SaveableText(t)
  }
  implicit object BoolSaveable extends Saveable[Boolean] {
    def toSaveable(t: Boolean) = SaveableText(t.toString)
  }
  implicit object TIDSaveable extends Saveable[TID] {
    def toSaveable(t: TID) = SaveableText(t.underlying)
  }
  implicit object ThingInfoSaveable extends Saveable[ThingInfo] {
    def toSaveable(t: ThingInfo) = SaveableText(t.oid.underlying)
  }
}

case class SaveablePropVal(propId: TID, v: SaveableVal)
