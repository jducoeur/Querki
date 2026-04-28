package querki.persistence

import querki.util.QLog

/**
 * Wrapper type, for fields that get added to an Event that has already been serialized
 * into the database.
 *
 * This is a challenging situation, because deserializing old Events will leave this field
 * null. So in order to deal with that, we have this hand-crafted type, which doesn't expose
 * any concrete methods but instead provides a couple of safe methods through an implicit
 * pimped class.
 *
 * AddedField itself is simply an empty marker trait -- the actual contents are held in the private
 * AddedFieldImpl. That's intentional, so that you can't accidentally get a hold of the contents
 * directly.
 *
 * Access to an AddedField is, thus, intentionally limited -- you can only use the functions that
 * are defined in AddedField.AddedFieldMethods. But it's null-safe, and thus works for schema evolution.
 */
sealed trait AddedField[T]

private[persistence] case class AddedFieldImpl[T](__content: T) extends AddedField[T]

object AddedField {

  implicit class AddedFieldMethods[T](af: AddedField[T]) {

    private def handle[U](name: String)(ifNull: => U)(ifFound: T => U): U =
      if (af == null) {
        QLog.logTrace(s"AddedField.$name: found null")
        ifNull
      } else {
        af match {
          case AddedFieldImpl(t) => {
            QLog.logTrace(s"AddedField.$name: found added value $t")
            ifFound(t)
          }
          case other => {
            QLog.logError(s"AddedField.$name: found unexpected value $other")
            ifNull
          }
        }
      }

    def getOrElse(default: T): T =
      handle("getOrElse")(default)(t => t)

    def map[R](f: T => R): Option[R] =
      handle("map")(Option.empty[R])(t => Some(f(t)))

    def isEmpty: Boolean = af == null
    def isDefined: Boolean = !isEmpty

    def toOption: Option[T] =
      handle("toOption")(Option.empty[T])(t => Some(t))
  }

  implicit def t2AddedField[T](t: T): AddedField[T] = AddedFieldImpl(t)
}
