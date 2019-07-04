package querki.test.mid

import querki.data.{ThingInfo, TID}

/**
 * Describes a value that can be "saved" through the Client API.
 */
trait SaveableVal {
  def toSave: List[String]
}

case class SaveableText(str: String) extends SaveableVal {
  def toSave = List(str)
}

case class SaveableTextList(strs: List[String]) extends SaveableVal {
  def toSave = strs
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
  implicit def ListSaveable[T: Saveable] = new Saveable[List[T]] {
    def toSaveable(ts: List[T]) = {
      val contents: List[String] = ts.map(_.toSaveable).map(_.toSave).flatten
      SaveableTextList(contents)
    }
  }
  
  implicit class SaveableOps[T : Saveable](v: T) {
    def toSaveable: SaveableVal = implicitly[Saveable[T]].toSaveable(v)
  }
}

case class SaveablePropVal(propId: TID, v: SaveableVal)
