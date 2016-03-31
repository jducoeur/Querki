package querki

import querki.globals._

import querki.data.UserInfo

package object identity {
  trait UserAccess extends EcologyInterface {
    def setUser(user:Option[UserInfo])
    
    def user:Option[UserInfo]
    
    def name:String
    
    def login():Future[UserInfo]
  }
}
