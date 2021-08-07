package querki

import models.Property

import querki.ecology._

import querki.core.QLText

package object conventions {

  object MOIDs extends EcotIds(15) {
    // Old OIDs, moved to here:
    val PropSummaryOID = sysId(85)
    val PropDetailsOID = sysId(86)

    val PropDescriptionOID = moid(1)
    val PropCategoriesOID = moid(2)
  }

  trait Conventions extends EcologyInterface {
    def PropSummary: Property[QLText, String]
    def PropDetails: Property[QLText, String]
  }
}
