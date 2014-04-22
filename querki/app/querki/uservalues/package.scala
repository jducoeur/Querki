package querki

import models.PType

import querki.ecology._

package object uservalues {
  trait UserValues extends EcologyInterface {
    def RatingType:PType[_]
    
    /**
     * If this is a UserValue wrapping type, this returns the underlying type.
     */
    def getUserType(pt:PType[_]):Option[PType[_]]
  }
}