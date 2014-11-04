package querki

import models.{Thing}

import querki.globals._

import querki.data.{IdentityInfo, PropValInfo, RequestInfo, ThingInfo}
import querki.identity.PublicIdentity
import querki.pages.PageDetails
import querki.values.RequestContext

package object api {
  trait ClientApi extends EcologyInterface {
    def requestInfo(rc:RequestContext):RequestInfo
    
    def thingInfo(t:Thing, rc:RequestContext):ThingInfo
    
    def identityInfo(identity:PublicIdentity):IdentityInfo
    
    def propValInfo(t:Thing, rc:RequestContext):Seq[PropValInfo]
  }
}
