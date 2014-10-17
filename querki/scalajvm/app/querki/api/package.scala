package querki

import models.{Thing}

import querki.globals._

import querki.data.{RequestInfo, ThingInfo}
import querki.pages.PageDetails
import querki.values.RequestContext

package object api {
  trait ClientApi extends EcologyInterface {
    def requestInfo(rc:RequestContext):RequestInfo
    
    def thingInfo(t:Thing, rc:RequestContext):ThingInfo
  }
}
