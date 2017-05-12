package querki.publication

import autowire._

import querki.data._
import querki.globals._
import querki.pages.Page

class PublicationEcot(e:Ecology) extends ClientEcot(e) with Publication  {
  
  def implements = Set(classOf[Publication])
  
  lazy val Client = interface[querki.client.Client]
  lazy val DataAccess = interface[querki.data.DataAccess]
  lazy val DataSetting = interface[querki.data.DataSetting]
  lazy val Pages = interface[querki.pages.Pages]
  
  lazy val std = DataAccess.std
  
  lazy val editPublicationFactory = Pages.registerThingPageFactory("_editPublish", { (params) => new EditPublicationPage(params) }, "modelId")
  
  override def postInit() = {
    editPublicationFactory
  }
  
  def isPublishable(thing:ThingInfo):Boolean = !thing.isModel && thing.hasFlag(std.publication.publishableProp)
  def isPublished(thing:ThingInfo):Boolean = thing.hasFlag(std.publication.publishedProp)
  def hasUnpublishedChanges(thing:ThingInfo):Boolean = thing.hasFlag(std.publication.hasUnpublishedChangesProp)
  
  def spaceHasPublications(thing:ThingInfo):Boolean = thing.hasFlag(std.publication.spaceHasPublicationsProp)

  def publish(thing:ThingInfo):Future[Page] = {
    Client[PublicationFunctions].publish(thing.oid).call().flatMap { newInfo =>
      DataSetting.setThing(Some(newInfo))
      Pages.thingPageFactory.showPage(newInfo)
    }    
  }
  
  def update(thing:ThingInfo, minor:Boolean):Future[Page] = {
    Client[PublicationFunctions].update(thing.oid, minor).call().flatMap { newInfo =>
      DataSetting.setThing(Some(newInfo))
      Pages.thingPageFactory.showPage(newInfo)
    }    
  }
}
