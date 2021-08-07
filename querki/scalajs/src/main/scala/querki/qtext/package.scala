package querki

import querki.globals._

package object qtext {

  trait QTextUtils extends EcologyInterface {
    def adjustUrl(urlIn: String): String
  }

}
