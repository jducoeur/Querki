package querki

import rx._

import querki.ecology._
import querki.pages.PageFactory

/**
 * @author jducoeur
 */
package object apps {
  trait Apps extends EcologyInterface {
    def appMgmtFactory:PageFactory
    def extractAppFactory:PageFactory
    
    def useApp()(implicit ctx:Ctx.Owner):Unit
  }
}
