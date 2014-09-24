package querki.data

import upickle._

import querki.globals._

class ClientDataEcot(e:Ecology) extends ClientEcot(e) with DataAccess with DataSetting {
  
  def implements = Set(classOf[DataAccess], classOf[DataSetting])
  
  lazy val UserAccess = interface[querki.identity.UserAccess]
  
  var _request:Option[RequestInfo] = None
  def request = _request.get
  def space = _request.flatMap(_.space)
  def mainThing = _request.flatMap(_.thing)
  
  @JSExport
  def unpickleRequest(pickled:String) = {
    _request = Some(read[RequestInfo](pickled))
    UserAccess.setUser(request.user)
  }
}
