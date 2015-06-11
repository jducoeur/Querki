package querki

import scala.concurrent.Future

import models.{Thing}

import querki.globals._

import querki.data._
import querki.identity.PublicIdentity
import querki.pages.PageDetails
import querki.session.messages.ClientAnswer
import querki.values.RequestContext

package object api {
  trait ClientApi extends EcologyInterface {
    def requestInfo(rc:RequestContext)(implicit state:SpaceState):RequestInfo
    
    def thingInfo(t:Thing, rc:RequestContext)(implicit state:SpaceState):ThingInfo
    
    def identityInfo(identity:PublicIdentity):IdentityInfo
    
    def propInfo(prop:AnyProp, rc:RequestContext)(implicit state:SpaceState):PropInfo
    
    def propValInfo(t:Thing, rc:RequestContext)(implicit state:SpaceState):Seq[PropValInfo]
    
    def handleCommonFunction(rc:RequestContext, req:autowire.Core.Request[String]):Future[ClientAnswer]
  }
}
