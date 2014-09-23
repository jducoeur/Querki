package querki

import models.{Thing}

import querki.global._

import querki.values.RequestContext

package object api {
  trait ClientApi extends EcologyInterface {
    /**
     * Render the request into a form suitable for passing through to the client.
     */
    def pickleRequest(rc:RequestContext):String
  }
}
