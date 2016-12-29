package querki

import querki.ecology._
import querki.pages.PageFactory

/**
 * @author jducoeur
 */
package object apps {
  trait Apps extends EcologyInterface {
    def appMgmtFactory:PageFactory
    def extractAppFactory:PageFactory
    
    def useApp():Unit
  }
}