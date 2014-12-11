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
    def requestInfo(rc:RequestContext):RequestInfo
    
    def thingInfo(t:Thing, rc:RequestContext):ThingInfo
    
    def identityInfo(identity:PublicIdentity):IdentityInfo
    
    def propInfo(prop:AnyProp, rc:RequestContext):PropInfo
    
    def propValInfo(t:Thing, rc:RequestContext):Seq[PropValInfo]
    
    def handleCommonFunction(req:autowire.Core.Request[String]):Future[ClientAnswer]
  }
}
