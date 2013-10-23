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
    def string(name:String) = row.get[String](name).get
    def oid(name:String) = OID(row.get[Long](name).get)
    def int(name:String) = row.get[Int](name).get
    def long(name:String) = row.get[Long](name).get
  }
}
