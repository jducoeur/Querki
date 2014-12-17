package querki.api

import querki.globals._

import querki.data.ThingInfo
import querki.values.RequestContext

class PassthroughHandler(val ecology:Ecology, rc:RequestContext) extends EcologyMember {
  val ClientApi = interface[ClientApi]
  val System = interface[querki.system.System]
  
  implicit val state = System.State
  
  var contents = Map.empty[String, ThingInfo]
  
  def pass(name:String) = {
    state.anythingByName(name) match {
      case Some(t) => contents += (name -> ClientApi.thingInfo(t, rc + state))
      case None => QLog.error(s"Attempting to send unknown Standard Thing $name")
    }
  }
}

class CommonFunctionsImpl(val ecology:Ecology, rc:RequestContext) extends CommonFunctions with EcologyMember
{
  def getStandardInfo():StandardInfo = {
    StandardInfo(
      querki.conventions.MOIDs.PropDetailsOID.toThingId,
      querki.conventions.MOIDs.PropSummaryOID.toThingId,
      querki.core.MOIDs.UrPropOID.toThingId,
      querki.core.MOIDs.NameOID.toThingId,
      querki.core.MOIDs.CollectionPropOID.toThingId,
      querki.core.MOIDs.TypePropOID.toThingId,
      querki.basic.MOIDs.SimpleThingOID.toThingId,
      querki.core.MOIDs.IsModelOID.toThingId,
      querki.basic.MOIDs.DisplayNameOID.toThingId,
      querki.editing.MOIDs.InstanceEditPropsOID.toThingId
    )
  }
  
  def getStandardThings():Map[String, ThingInfo] = {
    val passthrough = new PassthroughHandler(ecology, rc)
    val translator = new StandardThings(passthrough)
    val toucher = translator.touchEverything()
    passthrough.contents
  }
}
