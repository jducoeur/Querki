package querki

import models.{Thing}

import querki.global._

package object api {
  trait ClientApi extends EcologyInterface {
    /**
     * Render the given Thing to a form suitable for sending to the Client. This pickles the Thing
     * as a ThingInfo.
     */
    def pickleThing(t:Option[Thing]):String
  }
}