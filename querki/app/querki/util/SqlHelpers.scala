package querki.util

import anorm._

import models.OID

object SqlHelpers {
  /**
   * A simple pimped type to make SqlRow slightly less unpleasant to use.
   * 
   * Note that these methods will all throw exceptions if the column isn't found! This is not designed
   * to be gentle if the code and DB have somehow gotten out of sync.
   */
  implicit class EnhancedSqlRow(row:SqlRow) {
    def load[A](name:String)(implicit c:anorm.Column[A]) = row.get[A](name).get
    def opt[A](name:String)(implicit c:anorm.Column[A]) = load[Option[A]](name)
    
    def string(name:String) = load[String](name)
    def oid(name:String) = OID(row.get[Long](name).get)
    def optOid(name:String) = row.get[Option[Long]](name).get.map(OID(_))
    def int(name:String) = load[Int](name)
    def long(name:String) = load[Long](name)
    def bool(name:String) = load[Boolean](name)
  }
}
