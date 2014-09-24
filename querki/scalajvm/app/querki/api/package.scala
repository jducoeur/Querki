package querki

import models.{Thing}

import querki.global._

import querki.data.ThingInfo
import querki.values.RequestContext

package object api {
  trait ClientApi extends EcologyInterface {
    /**
     * Render the request into a form suitable for passing through to the client.
     */
    def pickleRequest(rc:RequestContext):String
    
    def thingInfo(topt:Option[Thing], rc:RequestContext):Option[ThingInfo]
  }
}
