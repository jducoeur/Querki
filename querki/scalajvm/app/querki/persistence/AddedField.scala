package querki.persistence

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
 * Access to an AddedField is, thus, intentionally limited -- you can use getOrElse
 * and map, and that's it. But it's null-safe, and thus works for schema evolution.
 */
sealed trait AddedField[T]

private [persistence] case class AddedFieldImpl[T](__content:T) extends AddedField[T] 

object AddedField {
  implicit class AddedFieldMethods[T](af:AddedField[T]) {
    def getOrElse(default:T):T = 
      if (af == null) 
        default 
      else {
        af match {
          case AddedFieldImpl(c) => c
        }
      }
    def map[R](f:T => R):Option[R] = 
      if (af == null) 
        None 
      else {
        af match {
          case AddedFieldImpl(c) =>  Some(f(c))
        }
      }
  }
  
  implicit def t2AddedField[T](t:T):AddedField[T] = AddedFieldImpl(t)
}
