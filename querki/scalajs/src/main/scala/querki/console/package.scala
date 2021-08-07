package querki

import querki.globals._
import querki.pages.PageFactory

package object console {

  trait Console extends EcologyInterface {
    def consoleFactory: PageFactory
  }
}
