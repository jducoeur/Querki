package querki.data

import upickle._

import querki.globals._

class ClientDataEcot(e:Ecology) extends ClientEcot(e) with DataAccess with DataSetting {
  
  def implements = Set(classOf[DataAccess], classOf[DataSetting])
  
  var currentThing:Option[ThingInfo] = None
  
  def mainThing = currentThing
  def setMainThing(topt:Option[ThingInfo]):Unit = { 
    currentThing = topt
  }
  
  @JSExport
  def setMainThing(pickled:String):Unit = {
    val thing = read[Option[ThingInfo]](pickled)
    setMainThing(thing)
  }
}
