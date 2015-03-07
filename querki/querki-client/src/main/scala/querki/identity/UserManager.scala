package querki.identity

import upickle._

import querki.globals._

import querki.data.UserInfo

class UserManagerEcot(e:Ecology) extends ClientEcot(e) with UserAccess {
  
  def implements = Set(classOf[UserAccess])
  
  var _user:Option[UserInfo] = None
  def user:Option[UserInfo] = _user
  
  def setUser(user:Option[UserInfo]) = _user = user
  
  def name = _user.map(_.mainIdentity.name).getOrElse("Not logged in")
}
