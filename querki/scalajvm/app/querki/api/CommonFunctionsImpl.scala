package querki.api

import scala.concurrent.Future

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
      case Some(t) => contents += (name -> ClientApi.thingInfo(t, rc))
      case None => QLog.error(s"Attempting to send unknown Standard Thing $name")
    }
  }
}

class CommonFunctionsImpl(info:AutowireParams)(implicit e:Ecology) extends AutowireApiImpl(info, e) with CommonFunctions
{ 
  def doRoute(req:Request):Future[String] = route[CommonFunctions](this)(req)

  def getStandardThings():Map[String, ThingInfo] = {
    val passthrough = new PassthroughHandler(ecology, rc)
    val translator = new StandardThings(passthrough)
    val toucher = translator.touchEverything()
    passthrough.contents
  }
}
