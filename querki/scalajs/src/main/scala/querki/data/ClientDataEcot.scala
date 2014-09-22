package querki.data

import querki.globals._

class ClientDataEcot(e:Ecology) extends ClientEcot(e) with DataAccess with DataSetting {
  
  def implements = Set(classOf[DataAccess], classOf[DataSetting])
  
  var currentThing:Option[ThingInfo] = None
  
  def mainThing = currentThing
  def setMainThing(thing:ThingInfo) = { currentThing = Some(thing) }
}
