package querki

import querki.globals._
import querki.pages._

package object admin {
  
  trait Admin extends EcologyInterface {
    def statisticsFactory:PageFactory
    def manageUsersFactory:PageFactory
    def monitorFactory:PageFactory
    def spacesTimingFactory:PageFactory
    def spaceTimingFactory:PageFactory
  }

}
