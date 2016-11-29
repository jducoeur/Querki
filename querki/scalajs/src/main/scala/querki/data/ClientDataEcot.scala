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
  var _space:Option[SpaceInfo] = None
  def space = _space.orElse(_request.flatMap(_.space))
  
  var mainThing:Option[ThingInfo] = None
  var mainModel:Option[ThingInfo] = None
  
  // TODO: this is horrible! setThing is used to define the "main" Thing, and we're using that
  // as a fallback in various places, but it's broadly evil, because it is so side-effecty. It is
  // easy to fail to set this, and wind up with cryptic bugs as a result.
  // We should really get rid of setThing() and .mainThing, and force everything currently depending
  // on them to be more explicit instead.
  def setThing(thing:Option[ThingInfo]) = mainThing = thing
  def setModel(model:Option[ThingInfo]) = mainModel = model
  def setSpace(space:Option[SpaceInfo]) = _space = space
  
  val standardThingPromise = Promise[StandardThings]
  def standardThings:Future[StandardThings] = standardThingPromise.future
  
  // NOTE: this generates a spurious error in Eclipse, because it's generated code.
  // Theoretically, we could get rid of this error as described in:
  //   https://github.com/sbt/sbt-buildinfo
  // But in practice that seems to screw up the client/server shared code.
  // TODO: figure out a way to suppress this error.
  def querkiVersion:String = querki.BuildInfo.version
  
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
