package querki

import querki.ecology._
import querki.globals._
import querki.pages.PageFactory

package object history {
  trait History extends EcologyInterface {
    def historySummaryFactory:PageFactory
  }
}
