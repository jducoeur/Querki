package querki

import models.{PType, PTypeBuilder}

import com.github.nscala_time.time.Imports._

import querki.ecology._

package object time {
  object MOIDs extends EcotIds(5) {
    val DateTimeTypeOID = moid(1)
    val ModifiedTimeMethodOID = moid(2)
  }
  
  type DateTime = com.github.nscala_time.time.Imports.DateTime
  val DateTime = com.github.nscala_time.time.Imports.DateTime
  
  // The epoch, typically used for "We don't really have a time for this":
  val epoch = new DateTime(0)
  
  trait Time extends EcologyInterface {
    def QDateTime:PType[DateTime] with PTypeBuilder[DateTime, DateTime]
  }
}