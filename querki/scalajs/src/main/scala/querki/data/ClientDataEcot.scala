package querki.data

import upickle._

import querki.globals._

class ClientDataEcot(e:Ecology) extends ClientEcot(e) with DataAccess with DataSetting {
  
  def implements = Set(classOf[DataAccess], classOf[DataSetting])
  
  lazy val UserAccess = interface[querki.identity.UserAccess]
  
  var _request:Option[RequestInfo] = None
  def request = _request.get
  def space = _request.flatMap(_.space)
  
  var mainThing:Option[ThingInfo] = None
  var mainModel:Option[ThingInfo] = None
  
  def setThing(thing:Option[ThingInfo]) = mainThing = thing
  def setModel(model:Option[ThingInfo]) = mainModel = model
  
  @JSExport
  def unpickleRequest(pickled:String) = {
    _request = Some(read[RequestInfo](pickled))
    UserAccess.setUser(request.user)
  }
}
