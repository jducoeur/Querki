package querki

import querki.ecology.EcologyInterface

/**
 * @author jducoeur
 */
package object collections {
  
  val CollTag = "Collection Manipulation"

  trait Collections extends EcologyInterface {
    def PositionsMethod:querki.core.MethodDefs#AbstractFunction
  }

}
