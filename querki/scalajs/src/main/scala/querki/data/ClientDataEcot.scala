package querki.data

import upickle._

import querki.globals._

class ClientDataEcot(e:Ecology) extends ClientEcot(e) with DataAccess with DataSetting {
  
  def implements = Set(classOf[DataAccess], classOf[DataSetting])
  
  lazy val UserAccess = interface[querki.identity.UserAccess]
  
  var space:Option[ThingInfo] = None
  var mainThing:Option[ThingInfo] = None
  
  @JSExport
  def unpickleRequest(pickled:String) = {
    val request = read[RequestInfo](pickled)
    mainThing = request.thing
    space = request.space
    UserAccess.setUser(request.user)
  }
}
