package querki

import models._

import querki.core.QLText
import querki.ecology._
import querki.types._

/**
 * @author jducoeur
 */
package object location {

  trait Location extends EcologyInterface {
    def LocationType: PType[ModeledPropertyBundle] with PTypeBuilder[ModeledPropertyBundle, SimplePropertyBundle]

    def StreetAddressProp: Property[QLText, String]
    def TownProp: Property[QLText, String]
    def StateProp: Property[QLText, String]
  }
}
