package querki

import scala.concurrent.Future

import models.{Thing, Wikitext}

import querki.ecology._
import querki.values.{RequestContext, SpaceState}

/**
 * This package represents the *abstraction* of a user interface. Note the contrast with querki.html,
 * which is specifically the HTML version of the UI.
 *
 * In the medium term, querki.html should probably get moved under here.
 */
package object ui {

  /**
   * Represents a specific way of rendering things. Note that this is *not* an EcologyInterface --
   * instead, you access the appropriate Renderer via the RequestContext.
   */
  trait UIRenderer {

    def renderThingDefault(
      thing: Thing
    )(implicit
      rc: RequestContext,
      state: SpaceState
    ): Future[Wikitext]
  }
}
