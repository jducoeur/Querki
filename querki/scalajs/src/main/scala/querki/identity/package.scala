package querki

import querki.globals._
import querki.data.UserInfo
import querki.display.ButtonGadget
import querki.pages.{Page, PageFactory}

package object identity {
  trait Identity extends EcologyInterface {
    def tosFactory:PageFactory
  }
  
  trait UserAccess extends EcologyInterface {
    def setUser(user:Option[UserInfo])
    
    def user:Option[UserInfo]
    
    def isActualUser:Boolean
    
    def loggedIn:Boolean = user.isDefined
    
    def name:String
    
    def login():Future[Page]
    def loginCore():Future[Unit]
    
    def resendActivationButton:ButtonGadget
  }
}
