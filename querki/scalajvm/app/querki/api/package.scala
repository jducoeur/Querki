package querki

import models.{Thing}

import querki.global._

import querki.values.RequestContext

package object api {
  trait ClientApi extends EcologyInterface {
    /**
     * Render the given Thing to a form suitable for sending to the Client. This pickles the Thing
     * as a ThingInfo.
     */
    def pickleThing(t:Option[Thing]):String
    
    /**
     * Renders the current User (fetched from the RC) into pickled form. The end result will be an
     * Option[UserInfo].
     */
    def pickleMe(t:RequestContext):String
  }
}
