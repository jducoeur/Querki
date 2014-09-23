package querki.identity

import upickle._

import querki.globals._

import querki.data.UserInfo

class UserManagerEcot(e:Ecology) extends ClientEcot(e) with UserAccess {
  
  def implements = Set(classOf[UserAccess])
  
  var _user:Option[UserInfo] = None
  
  @JSExport
  def setUser(pickled:String) = _user = read[Option[UserInfo]](pickled)
  
  def name = _user.map(_.mainIdentity.name).getOrElse("Not logged in")
}
