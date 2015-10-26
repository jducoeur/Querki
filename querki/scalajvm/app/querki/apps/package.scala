package querki

import models.Property

import querki.globals._

/**
 * @author jducoeur
 */
package object apps {
  trait Apps extends EcologyInterface {
    def CanManipulateAppsPerm:Property[OID,OID]
  }
}