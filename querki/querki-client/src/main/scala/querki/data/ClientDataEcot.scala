package querki.data

import scala.concurrent.Future

import upickle._
import autowire._

import querki.globals._

import querki.api.{CommonFunctions, PassthroughHandler, StandardThings, ThingFunctions}

class ClientDataEcot(e:Ecology) extends ClientEcot(e) with DataAccess with DataSetting {
  
  def implements = Set(classOf[DataAccess], classOf[DataSetting])

  lazy val Client = interface[querki.client.Client]
  lazy val UserAccess = interface[querki.identity.UserAccess]
  
  var _request:Option[RequestInfo] = None
  def request = _request.get
  def space = _request.flatMap(_.space)
  
  var mainThing:Option[ThingInfo] = None
  var mainModel:Option[ThingInfo] = None
  
  def setThing(thing:Option[ThingInfo]) = mainThing = thing
  def setModel(model:Option[ThingInfo]) = mainModel = model
  
  val standardThingPromise = Promise[StandardThings]
  def standardThings:Future[StandardThings] = standardThingPromise.future
  
  override def postInit() = {
    Client[CommonFunctions].getStandardThings().call().foreach { thingMap =>
      val handler = new PassthroughHandler(thingMap)
      val things = new StandardThings(handler)
      standardThingPromise.success(things)
    }
  }
  
  /**
   * TODO: ClientDataEcot should be maintaining its own cache of ThingInfos, to speed
   * things up. The only reason we're not doing that yet is that it required that the Space
   * be versioned, and that we be invalidating our cache against the changes on the server.
   * That's a big enough deal to not do casually.
   * 
   * That said, once we *do* have such a cache, we might as well actually store it in the
   * Browser DB: go whole-hog and maintain enough info locally to seriously cut traffic.
   */
  def getThing(thingId:TID):Future[ThingInfo] = {
    Client[ThingFunctions].getThingInfo(thingId).call()
  }
  
  /**
   * TODO: we should be caching this list, probably on a per-Space basis.
   */
  def getAllProps():Future[SpaceProps] = {
    Client[ThingFunctions].getAllProperties().call()
  }
  
  /**
   * TODO: we should be caching this list, probably on a per-Space basis.
   */
  def getAllTypes():Future[AllTypeInfo] = {
    Client[ThingFunctions].getAllTypes().call()
  }
  
  @JSExport
  def unpickleRequest(pickled:String) = {
    _request = Some(read[RequestInfo](pickled))
    UserAccess.setUser(request.user)
  }
}
