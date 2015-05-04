package querki

import models.{PType, PTypeBuilder}

import com.github.nscala_time.time._

import querki.ecology._

/**
 * At least for the time being, querki.time is mostly nscala-time with some tweaks.
 * Querki code should usually just import querki.time._.
 */
package object time extends Imports with Implicits {
  object MOIDs extends EcotIds(5) {
    val DateTimeTypeOID = moid(1)
    val ModifiedTimeMethodOID = moid(2)
    val DateTypeOID = moid(3)
  }
  
  // The epoch, typically used for "We don't really have a time for this":
  val epoch = new DateTime(0)
  
  trait Time extends EcologyInterface {
    def QDateTime:PType[DateTime] with PTypeBuilder[DateTime, DateTime]
  }
}
