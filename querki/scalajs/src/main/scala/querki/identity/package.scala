package querki

import querki.globals._
import querki.data.UserInfo
import querki.pages.Page

package object identity {
  trait UserAccess extends EcologyInterface {
    def setUser(user:Option[UserInfo])
    
    def user:Option[UserInfo]
    
    def name:String
    
    def login():Future[Page]
    def loginCore():Future[Unit]
  }
}
