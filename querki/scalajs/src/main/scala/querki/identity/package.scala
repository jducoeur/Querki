package querki

import querki.globals._

package object identity {
  trait UserAccess extends EcologyInterface {
    def name:String
  }
}
