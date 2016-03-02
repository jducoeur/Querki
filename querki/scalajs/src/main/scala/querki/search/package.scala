package querki

import querki.ecology._
import querki.globals._
import querki.pages.PageFactory

/**
 * @author jducoeur
 */
package object search {
  trait Search extends EcologyInterface {
    def searchResultsFactory:PageFactory
  }
}