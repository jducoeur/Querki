package querki

import querki.data._
import querki.globals._
import querki.pages.Page

package object publication {
  trait Publication extends EcologyInterface {
    def isPublishable(thing:ThingInfo):Boolean
    def isPublished(thing:ThingInfo):Boolean
    def hasUnpublishedChanges(thing:ThingInfo):Boolean
    
    def publish(thing:ThingInfo):Future[Page]
    def update(thing:ThingInfo, minor:Boolean):Future[Page]
  }
}
