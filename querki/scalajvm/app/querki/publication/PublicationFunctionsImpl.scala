package querki.publication

import models._
import querki.api.{SpaceApiImpl, AutowireParams}
import querki.data._
import querki.globals._
import querki.session.messages.ChangeProps2

import PublicationCommands._

class PublicationFunctionsImpl(info:AutowireParams)(implicit e:Ecology) extends SpaceApiImpl(info, e) with PublicationFunctions  {
  
  lazy val ClientApi = interface[querki.api.ClientApi]
  lazy val Publication = interface[Publication]
  
  def doRoute(req:Request):Future[String] = route[PublicationFunctions](this)(req)
  
  /**
   * For the time being, publish and update are so similar that they should share this code.
   * This *may* change later; we'll see.
   */
  private def doPublish(thing:Thing, cmd:PublicationCommand):Future[ThingInfo] = {
    spaceRouter.request(cmd).flatMap { case PublishResponse(updatedState) =>
      implicit val s = updatedState
      // TODO: what should we do in the weird case that the thing is no longer found? This is unlikely,
      // but could happen if, say, there was a race between publishing and deleting this Thing:
      val updatedThing = updatedState.anything(thing.id).get
      ClientApi.thingInfo(updatedThing, rc)
    }    
  }

  def publish(thingId:TID):Future[ThingInfo] = withThing(thingId) { thing =>
    doPublish(thing, Publish(user, List(thing.id), emptyProps, state))
  }
  
  def update(thingId:TID, minor:Boolean):Future[ThingInfo] = withThing(thingId) { thing =>
    val props =
      if (minor)
        toProps(Publication.MinorUpdateProp(true))
      else
        emptyProps
    doPublish(thing, Update(user, List(thing.id), props, state))
  }
  
  def changePublishedModels():Future[Unit] = {
    val hasPublishables = state.allModels.exists { model =>
      model.ifSet(Publication.PublishableModelProp)(state)
    }
    val props = toProps(Publication.SpaceHasPublications(hasPublishables))
    val msg = createSelfRequest(ChangeProps2(state.id, props))
    spaceRouter.request(msg).map { _ => ()}
  }
}
