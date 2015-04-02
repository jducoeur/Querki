package querki

import querki.globals._
import querki.pages._

package object admin {
  
  trait Admin extends EcologyInterface {
    def statisticsFactory:PageFactory
  }

}
