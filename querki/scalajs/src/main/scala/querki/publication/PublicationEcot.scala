package querki.publication

import scalatags.JsDom.all._
import autowire._

import querki.data._
import querki.display._
import querki.globals._
import querki.pages.Page

class PublicationEcot(e:Ecology) extends ClientEcot(e) with Publication  {
  
  def implements = Set(classOf[Publication])
  
  lazy val Client = interface[querki.client.Client]
  lazy val DataAccess = interface[querki.data.DataAccess]
  lazy val DataSetting = interface[querki.data.DataSetting]
  lazy val PageManager = interface[querki.display.PageManager]
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
  
  def discardChanges(thing:ThingInfo):Unit = {
    val confirmDialog = new Dialog("Confirm Discard",
      p(b(s"""Are you sure you want to discard changes to ${thing.displayName}? They can not be recovered afterwards.""".stripMargin)),
      (ButtonGadget.Warning, Seq(s"Discard changes to ${thing.displayName}", id := "_discardConfirm"), { dialog => 
        Client[PublicationFunctions].discardChanges(thing.oid).call().foreach { _ =>
          dialog.done()
          PageManager.reload()
        }
      }),
      (ButtonGadget.Normal, Seq("Cancel", id := "_cancelDiscard"), { dialog => dialog.done() })
    )
    confirmDialog.show()
  }
}
