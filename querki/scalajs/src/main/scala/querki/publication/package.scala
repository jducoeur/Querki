package querki

import querki.data._
import querki.globals._
import querki.pages.{Page, ThingPageFactory}

package object publication {

  trait Publication extends EcologyInterface {
    def editPublicationFactory: ThingPageFactory

    def isPublishable(thing: ThingInfo): Boolean
    def isPublished(thing: ThingInfo): Boolean
    def hasUnpublishedChanges(thing: ThingInfo): Boolean
    def spaceHasPublications(thing: ThingInfo): Boolean

    def publish(
      thing: ThingInfo,
      forceReload: Boolean
    ): Future[Page]

    def update(
      thing: ThingInfo,
      minor: Boolean,
      forceReload: Boolean
    ): Future[Page]

    def discardChanges(
      thing: ThingInfo,
      forceReload: Boolean
    ): Unit
  }
}
